-- | Defines the ReadModel abstraction for queryable persistent views.
--
-- A 'ReadModel' bundles everything needed to maintain an event-driven
-- persistent view: initialization, event handling, checkpointing, and reset.
--
-- The library manages the event-to-handler pipeline and checkpointing.
-- Users define their own schema, write the 'EventHandler' that populates
-- tables, and query via their own code (SQL, persistent, etc.).
module Eventium.ReadModel
  ( ReadModel (..),
    runReadModel,
    catchUpReadModel,
    rebuildReadModel,
    combineReadModels,
    readModelPublisher,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Eventium.EventHandler
import Eventium.EventPublisher (GlobalEventPublisher (..))
import Eventium.EventSubscription (CheckpointStore (..), PollingIntervalMillis)
import Eventium.Store.Class

-- | A read model that maintains a queryable persistent view from events.
--
-- Users provide:
--
-- * 'initialize' — idempotent setup (run migrations, create tables)
-- * 'eventHandler' — processes global stream events, writes to user-defined storage
-- * 'checkpointStore' — tracks the last processed 'SequenceNumber'
-- * 'reset' — drop the view's data (tables). The checkpoint is reset by
--   'rebuildReadModel', so 'reset' need only clear user-owned storage.
data ReadModel m event = ReadModel
  { initialize :: m (),
    eventHandler :: EventHandler m (GlobalStreamEvent event),
    checkpointStore :: CheckpointStore m SequenceNumber,
    reset :: m ()
  }

-- | Subscribe to the global event stream and keep the read model updated.
-- Runs forever, polling at the given interval.
runReadModel ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  PollingIntervalMillis ->
  ReadModel m event ->
  m ()
runReadModel globalReader pollIntervalMs rm = do
  rm.initialize
  forever $ pollReadModelOnce globalReader pollIntervalMs rm

-- | Bring a read model up to date from its current checkpoint, without
-- resetting it. Initializes, then replays @checkpoint+1 → latest@ and returns
-- once all currently-available events are processed.
--
-- This is the one-shot startup/backfill counterpart to 'runReadModel' (which
-- polls forever) and 'rebuildReadModel' (which resets first). Safe to run on a
-- persistent read model at boot: it never wipes durable state, and replays
-- nothing when the checkpoint is already current.
catchUpReadModel ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  ReadModel m event ->
  m ()
catchUpReadModel globalReader rm = do
  rm.initialize
  replayAll
  where
    replayAll = do
      latestSeq <- rm.checkpointStore.getCheckpoint
      newEvents <- globalReader.getEvents (eventsStartingAt () $ latestSeq + 1)
      case NE.nonEmpty newEvents of
        Nothing -> return ()
        Just ne -> do
          handleEvents rm.eventHandler newEvents
          rm.checkpointStore.saveCheckpoint (NE.last ne).position
          replayAll

-- | Reset the read model and replay all events from the beginning.
-- Returns after processing all currently available events.
rebuildReadModel ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  ReadModel m event ->
  m ()
rebuildReadModel globalReader rm = do
  rm.reset
  -- Reset the checkpoint we own, so the user's 'reset' only has to drop view
  -- data; then replay from the start.
  rm.checkpointStore.saveCheckpoint 0
  catchUpReadModel globalReader rm

-- | Combine multiple read models into one. Events are fanned out to all
-- handlers. Initialize and reset run all sub-models.
--
-- For independently checkpointed read models, prefer running them
-- separately with 'runReadModel'.
combineReadModels ::
  (Applicative m) =>
  [ReadModel m event] ->
  ReadModel m event
combineReadModels rms =
  ReadModel
    { initialize = traverse_ (.initialize) rms,
      eventHandler = foldMap (.eventHandler) rms,
      checkpointStore =
        CheckpointStore
          { getCheckpoint = case rms of
              [] -> pure 0
              (rm : _) -> rm.checkpointStore.getCheckpoint,
            saveCheckpoint = \sn -> traverse_ (\rm -> rm.checkpointStore.saveCheckpoint sn) rms
          },
      reset = traverse_ (.reset) rms
    }

-- | Drive a 'ReadModel' synchronously from a write's published global events:
-- apply its handler to the batch and advance its checkpoint to the last event's
-- global position. The synchronous counterpart to 'runReadModel' — wire it into
-- a 'publishingGlobalEventStoreWriter' / 'publishingGlobalTaggedCodecEventStoreWriter'
-- to project the read model in the write transaction (strong consistency).
--
-- The same 'ReadModel' value can therefore be run either asynchronously
-- ('runReadModel') or synchronously (here). 'initialize'/'reset'/'rebuildReadModel'
-- remain the lifecycle/backfill path.
readModelPublisher ::
  (Monad m) =>
  ReadModel m event ->
  GlobalEventPublisher m event
readModelPublisher rm = GlobalEventPublisher $ \events -> do
  handleEvents rm.eventHandler events
  case NE.nonEmpty events of
    Nothing -> return ()
    Just ne -> rm.checkpointStore.saveCheckpoint (NE.last ne).position

pollReadModelOnce ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  PollingIntervalMillis ->
  ReadModel m event ->
  m ()
pollReadModelOnce globalReader pollIntervalMs rm = do
  latestSeq <- rm.checkpointStore.getCheckpoint
  newEvents <- globalReader.getEvents (eventsStartingAt () $ latestSeq + 1)
  handleEvents rm.eventHandler newEvents
  case NE.nonEmpty newEvents of
    Nothing -> return ()
    Just ne -> rm.checkpointStore.saveCheckpoint (NE.last ne).position
  liftIO $ threadDelay (pollIntervalMs * 1000)
