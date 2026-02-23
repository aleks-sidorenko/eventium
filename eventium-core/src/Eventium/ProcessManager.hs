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
    runProcessManagerEffects,
    processManagerEventHandler,
  )
where

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

-- | A side effect that a 'ProcessManager' wants to perform. This is a pure
-- data type — it describes /what/ should happen, not /how/.
data ProcessManagerEffect command
  = -- | Issue a command to a specific aggregate (identified by 'UUID').
    IssueCommand UUID command
  deriving (Show, Eq)

-- | Execute a list of 'ProcessManagerEffect's using the provided command
-- dispatcher.
--
-- The command dispatcher is a plain function @UUID -> command -> m ()@
-- rather than a newtype, since commands are point-to-point (no fan-out)
-- and only used here.
runProcessManagerEffects ::
  (Monad m) =>
  (UUID -> command -> m ()) ->
  [ProcessManagerEffect command] ->
  m ()
runProcessManagerEffects dispatch = mapM_ go
  where
    go (IssueCommand uuid cmd) = dispatch uuid cmd

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
  (UUID -> command -> m ()) ->
  EventHandler m (VersionedStreamEvent event)
processManagerEventHandler pm globalReader dispatch = EventHandler $ \event -> do
  let globalProjection = globalStreamProjection (processManagerProjection pm)
  StreamProjection {..} <- getLatestStreamProjection globalReader globalProjection
  let effects = processManagerReact pm streamProjectionState event
  runProcessManagerEffects dispatch effects
