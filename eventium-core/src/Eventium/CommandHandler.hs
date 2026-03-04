-- | Defines a Command Handler type.
--
-- A 'CommandHandler' is the combination of a 'Projection' (to reconstruct
-- aggregate state from events) and a pure decision function that validates
-- a command against the current state. The decision function returns
-- @'Either' err [event]@: 'Right' with new events on success, or 'Left'
-- with a domain error on rejection.
module Eventium.CommandHandler
  ( CommandHandler (..),
    CommandHandlerError (..),
    applyCommandHandler,
    codecCommandHandler,
    embeddedCommandHandler,
  )
where

import Control.Exception (throw)
import Eventium.Codec
import Eventium.Projection
import Eventium.Store.Class
import Eventium.TypeEmbedding
import Eventium.UUID

-- | A 'CommandHandler' is a combination of a 'Projection' and a function to
-- validate commands against that 'Projection'. When using a command handler in
-- some service, it is common to simply load the latest projection state from
-- the event store and handle the command. If the command is valid then the new
-- events are applied to the projection in the event store.
--
-- The @err@ type parameter represents the domain error type returned when a
-- command is rejected.
data CommandHandler state event command err
  = CommandHandler
  { decide :: state -> command -> Either err [event],
    projection :: Projection state event
  }

-- | Errors that can occur when applying a command handler.
data CommandHandlerError err
  = -- | The command was rejected by the domain logic.
    CommandRejected err
  | -- | An optimistic concurrency conflict occurred when writing events.
    ConcurrencyConflict (EventWriteError EventVersion)
  deriving (Show, Eq)

-- | Loads the latest version of a 'Projection' from the event store and tries
-- to apply the 'CommandHandler' command to it. If the command is accepted,
-- the events are saved back to the store.
--
-- Returns @'Left' ('CommandRejected' err)@ if the domain logic rejects the
-- command, or @'Left' ('ConcurrencyConflict' ...)@ if the event store
-- position has changed since reading. Returns @'Right' events@ on success.
applyCommandHandler ::
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  VersionedEventStoreReader m event ->
  CommandHandler state event command err ->
  UUID ->
  command ->
  m (Either (CommandHandlerError err) [event])
applyCommandHandler writer reader cmdHandler uuid command = do
  sp <- getLatestStreamProjection reader (versionedStreamProjection uuid cmdHandler.projection)
  case cmdHandler.decide sp.state command of
    Left err -> return $ Left (CommandRejected err)
    Right events -> do
      result <- writer.storeEvents uuid (ExactPosition sp.position) events
      case result of
        Left writeErr -> return $ Left (ConcurrencyConflict writeErr)
        Right _ -> return $ Right events

-- | Use a pair of 'Codec's to wrap a 'CommandHandler' with event type
-- @event@ and command type @command@ so it uses the @encodedEvent@ and
-- @encodedCommand@ types.
--
-- Throws 'DecodeError' if the command cannot be decoded.
codecCommandHandler ::
  Codec event encodedEvent ->
  Codec command encodedCommand ->
  CommandHandler state event command err ->
  CommandHandler state encodedEvent encodedCommand err
codecCommandHandler eventCodec commandCodec cmdHandler =
  CommandHandler codecHandler codecProjection'
  where
    codecProjection' = codecProjection eventCodec cmdHandler.projection
    codecHandler st encodedCmd =
      case commandCodec.decode encodedCmd of
        Nothing -> throw $ DecodeError "codecCommandHandler" "Failed to decode command"
        Just cmd -> fmap (map eventCodec.encode) (cmdHandler.decide st cmd)

-- | Like 'codecCommandHandler' but uses 'TypeEmbedding's instead of
-- 'Codec's. Intended for embedding aggregate-specific event\/command
-- types into application-wide sum types.
--
-- The projection uses lenient extraction (non-matching events are skipped).
-- Non-matching commands return @'Right' []@ (no events produced), which
-- enables safe multi-aggregate command dispatching — callers can try
-- multiple embedded handlers in sequence without catching exceptions.
embeddedCommandHandler ::
  TypeEmbedding event adaptedEvent ->
  TypeEmbedding command adaptedCommand ->
  CommandHandler state event command err ->
  CommandHandler state adaptedEvent adaptedCommand err
embeddedCommandHandler eventEmb commandEmb cmdHandler =
  CommandHandler embeddedHandler embeddedProjection'
  where
    embeddedProjection' = embeddedProjection eventEmb cmdHandler.projection
    embeddedHandler st adaptedCmd =
      case commandEmb.extract adaptedCmd of
        Nothing -> Right []
        Just cmd -> fmap (map eventEmb.embed) (cmdHandler.decide st cmd)
