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

import qualified Data.Text as T
import Eventium.CommandHandler (CommandHandler, CommandHandlerError (..), applyCommandHandler)
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher, RejectionReason (..), mkCommandDispatcher)
import Eventium.Store.Class (VersionedEventStoreReader, VersionedEventStoreWriter)

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
        Left (ConcurrencyConflict _) -> pure (CommandFailed "Concurrency conflict")
        Right [] -> go rest uuid cmd
