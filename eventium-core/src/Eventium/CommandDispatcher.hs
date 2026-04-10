{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides list-based command routing for multi-aggregate systems.
--
-- Instead of manually trying each command handler in a nested cascade,
-- collect handlers into a list and let 'commandHandlerDispatcher' route
-- commands automatically.
module Eventium.CommandDispatcher
  ( AggregateHandler,
    mkAggregateHandler,
    mkAggregateHandlerWith,
    commandHandlerDispatcher,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Eventium.Codec (Codec)
import Eventium.CommandHandler (CommandHandler, CommandHandlerError (..), applyCommandHandler)
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher (..), RejectionReason (..))
import Eventium.Store.Class (EventStoreWriter, VersionedEventStoreReader, metadataEnrichingEventStoreWriterWithEnricher)
import Eventium.Store.Types (EventVersion, TaggedEvent)
import Eventium.UUID (UUID)

-- | An embedded command handler paired with an error formatter.
--
-- Erases the aggregate state type and error type so that multiple
-- aggregate handlers can be collected in a homogeneous list for routing.
data AggregateHandler event command
  = forall state err.
    AggregateHandler
      (CommandHandler state event command err)
      (err -> RejectionReason)

-- | Construct an 'AggregateHandler' using 'Show' to format errors.
mkAggregateHandler ::
  (Show err) =>
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandler h = AggregateHandler h (RejectionReason . T.pack . show)

-- | Construct an 'AggregateHandler' with an explicit error formatter.
mkAggregateHandlerWith ::
  (err -> RejectionReason) ->
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandlerWith fmt h = AggregateHandler h fmt

-- | Build a 'CommandDispatcher' from a list of 'AggregateHandler's.
--
-- Accepts a tagged writer and codec; the 'MetadataEnricher' supplied at
-- dispatch time is applied to create a per-dispatch enriched writer.
--
-- Tries each handler in order:
--
--   * @Right (e:es)@ — command matched and produced events → 'CommandSucceeded'
--   * @Left (CommandRejected err)@ — command matched but was rejected → 'CommandFailed'
--   * @Left (ConcurrencyConflict _)@ — optimistic locking failure → 'CommandFailed'
--   * @Right []@ — command did not match this handler → try next
--
-- If no handler matches (all return @Right []@), reports 'CommandSucceeded' (no-op).
commandHandlerDispatcher ::
  (MonadIO m, Typeable event) =>
  Codec event encoded ->
  EventStoreWriter UUID EventVersion m (TaggedEvent encoded) ->
  VersionedEventStoreReader m event ->
  [AggregateHandler event command] ->
  CommandDispatcher m command
commandHandlerDispatcher codec taggedWriter reader handlers =
  CommandDispatcher $ \uuid cmd enricher ->
    let writer = metadataEnrichingEventStoreWriterWithEnricher enricher codec taggedWriter
     in go handlers writer uuid cmd
  where
    go [] _ _ _ = pure CommandSucceeded
    go (AggregateHandler handler formatErr : rest) writer uuid cmd = do
      result <- applyCommandHandler writer reader handler uuid cmd
      case result of
        Right (_ : _) -> pure CommandSucceeded
        Left (CommandRejected err) -> pure (CommandFailed (formatErr err))
        Left (ConcurrencyConflict _) -> pure (CommandFailed "Concurrency conflict")
        Right [] -> go rest writer uuid cmd
