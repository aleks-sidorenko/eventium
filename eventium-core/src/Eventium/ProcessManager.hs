{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Defines a Process Manager (saga) abstraction for orchestrating
-- interactions across multiple event streams.
--
-- A 'ProcessManager' is the combination of a 'Projection' (to track state
-- across streams) and a pure @react@ function that decides what commands to
-- issue in response to each event. Commands are represented as
-- 'ProcessManagerEffect' values — a pure data type — which are then
-- executed by 'runProcessManagerEffects'.
--
-- This design cleanly separates the pure decision logic from effectful
-- execution, making process managers easy to unit test.
module Eventium.ProcessManager
  ( ProcessManager (..),
    ProcessManagerEffect (..),
    CommandDispatchResult (..),
    RejectionReason (..),
    CommandDispatcher (..),
    mkCommandDispatcher,
    fireAndForgetDispatcher,
    runProcessManagerEffects,
    processManagerEventHandler,
  )
where

import Control.Monad (void)
import Data.String (IsString)
import Data.Text (Text)
import Eventium.EventHandler (EventHandler (..))
import Eventium.Projection
import Eventium.Store.Class (GlobalEventStoreReader)
import Eventium.Store.Types
import Eventium.UUID

-- | A 'ProcessManager' manages interaction between event streams. It
-- listens to events and decides what commands to issue to other aggregates.
--
-- * 'processManagerProjection' — a pure fold over versioned stream events
--   that tracks the process manager's state.
-- * 'processManagerReact' — a pure function that, given the current state
--   and a new event, returns a list of effects to execute.
data ProcessManager state event command = ProcessManager
  { processManagerProjection :: Projection state (VersionedStreamEvent event),
    processManagerReact :: state -> VersionedStreamEvent event -> [ProcessManagerEffect command]
  }

-- | A typed wrapper for the reason a command was rejected.
--
-- Use 'RejectionReason' instead of raw 'Text' to avoid accidentally
-- mixing rejection reasons with other textual values at the dispatch boundary.
newtype RejectionReason = RejectionReason {unRejectionReason :: Text}
  deriving (Show, Eq, Ord, IsString)

-- | A side effect that a 'ProcessManager' wants to perform. This is a pure
-- data type — it describes /what/ should happen, not /how/.
data ProcessManagerEffect command
  = -- | Issue a command to a specific aggregate (identified by 'UUID').
    IssueCommand UUID command
  | -- | Issue a command with compensation: if the command fails, execute
    -- the compensation effects produced by the failure handler.
    --
    -- The 'RejectionReason' argument to the compensation function is the failure reason
    -- from 'CommandFailed'.
    IssueCommandWithCompensation UUID command (RejectionReason -> [ProcessManagerEffect command])

instance (Show command) => Show (ProcessManagerEffect command) where
  show (IssueCommand uuid cmd) = "IssueCommand " ++ show uuid ++ " " ++ show cmd
  show (IssueCommandWithCompensation uuid cmd _) =
    "IssueCommandWithCompensation " ++ show uuid ++ " " ++ show cmd ++ " <compensation>"

instance (Eq command) => Eq (ProcessManagerEffect command) where
  IssueCommand u1 c1 == IssueCommand u2 c2 = u1 == u2 && c1 == c2
  IssueCommandWithCompensation u1 c1 _ == IssueCommandWithCompensation u2 c2 _ = u1 == u2 && c1 == c2
  _ == _ = False

-- | Result of dispatching a command to an aggregate.
data CommandDispatchResult
  = -- | The command was accepted and events were stored.
    CommandSucceeded
  | -- | The command was rejected by the aggregate with a reason.
    CommandFailed RejectionReason
  deriving (Show, Eq)

-- | A command dispatcher routes commands to aggregates and reports the outcome.
--
-- Use 'mkCommandDispatcher' to construct one from a dispatch function.
-- Use 'fireAndForgetDispatcher' to adapt a legacy @UUID -> command -> m ()@
-- callback that does not report failures.
newtype CommandDispatcher m command = CommandDispatcher
  { dispatchCommand :: UUID -> command -> m CommandDispatchResult
  }

-- | Construct a 'CommandDispatcher' from a dispatch function.
mkCommandDispatcher ::
  (UUID -> command -> m CommandDispatchResult) ->
  CommandDispatcher m command
mkCommandDispatcher = CommandDispatcher

-- | Adapt a legacy fire-and-forget dispatcher into a 'CommandDispatcher'
-- that always reports 'CommandSucceeded'.
fireAndForgetDispatcher ::
  (Monad m) =>
  (UUID -> command -> m ()) ->
  CommandDispatcher m command
fireAndForgetDispatcher f = CommandDispatcher $ \uuid cmd ->
  f uuid cmd >> pure CommandSucceeded

-- | Execute a list of 'ProcessManagerEffect's using the provided
-- 'CommandDispatcher'.
runProcessManagerEffects ::
  (Monad m) =>
  CommandDispatcher m command ->
  [ProcessManagerEffect command] ->
  m ()
runProcessManagerEffects dispatcher = mapM_ go
  where
    go (IssueCommand uuid cmd) =
      void $ dispatchCommand dispatcher uuid cmd
    go (IssueCommandWithCompensation uuid cmd onFailure) = do
      result <- dispatchCommand dispatcher uuid cmd
      case result of
        CommandSucceeded -> pure ()
        CommandFailed reason -> mapM_ go (onFailure reason)

-- | Create an 'EventHandler' that wires a 'ProcessManager' to a global
-- event store reader and a command dispatcher.
--
-- For each incoming event:
--
--   1. Rebuilds the process manager state from the global event stream
--   2. Calls 'processManagerReact' with the current state and the new event
--   3. Executes the resulting effects via the dispatcher
processManagerEventHandler ::
  (Monad m) =>
  ProcessManager state event command ->
  GlobalEventStoreReader m event ->
  CommandDispatcher m command ->
  EventHandler m (VersionedStreamEvent event)
processManagerEventHandler pm globalReader dispatcher = EventHandler $ \event -> do
  let globalProjection = globalStreamProjection (processManagerProjection pm)
  StreamProjection {..} <- getLatestStreamProjection globalReader globalProjection
  let effects = processManagerReact pm streamProjectionState event
  runProcessManagerEffects dispatcher effects
