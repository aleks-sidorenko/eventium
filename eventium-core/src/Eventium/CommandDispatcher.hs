{-# LANGUAGE ExistentialQuantification #-}

-- | Provides list-based command routing for multi-aggregate systems.
--
-- Instead of manually trying each command handler in a nested cascade,
-- collect handlers into a list and let 'commandHandlerDispatcher' route
-- commands automatically.
module Eventium.CommandDispatcher
  ( AggregateHandler,
    mkAggregateHandler,
    commandHandlerDispatcher,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Eventium.CommandHandler (CommandHandler, CommandHandlerError (..), applyCommandHandler)
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher, mkCommandDispatcher)
import Eventium.Store.Class (VersionedEventStoreReader, VersionedEventStoreWriter)

-- | An embedded command handler paired with an error formatter.
--
-- Erases the aggregate state type and error type so that multiple
-- aggregate handlers can be collected in a homogeneous list for routing.
data AggregateHandler event command
  = forall state err.
    AggregateHandler
      (CommandHandler state event command err)
      (err -> Text)

-- | Construct an 'AggregateHandler' from a command handler and an error
-- formatting function.
mkAggregateHandler ::
  CommandHandler state event command err ->
  (err -> Text) ->
  AggregateHandler event command
mkAggregateHandler = AggregateHandler

-- | Build a 'CommandDispatcher' from a list of 'AggregateHandler's.
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
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  VersionedEventStoreReader m event ->
  [AggregateHandler event command] ->
  CommandDispatcher m command
commandHandlerDispatcher writer reader handlers =
  mkCommandDispatcher $ \uuid cmd -> go handlers uuid cmd
  where
    go [] _ _ = pure CommandSucceeded
    go (AggregateHandler handler formatErr : rest) uuid cmd = do
      result <- applyCommandHandler writer reader handler uuid cmd
      case result of
        Right (_ : _) -> pure CommandSucceeded
        Left (CommandRejected err) -> pure (CommandFailed (formatErr err))
        Left (ConcurrencyConflict _) -> pure (CommandFailed (T.pack "Concurrency conflict"))
        Right [] -> go rest uuid cmd
