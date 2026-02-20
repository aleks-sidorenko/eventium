{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.Memory
  ( tvarEventStoreReader,
    tvarEventStoreWriter,
    tvarEventStoreWriterTagged,
    tvarGlobalEventStoreReader,
    stateEventStoreReader,
    stateEventStoreWriter,
    stateEventStoreWriterTagged,
    stateGlobalEventStoreReader,
    embeddedStateEventStoreReader,
    embeddedStateEventStoreWriter,
    embeddedStateEventStoreWriterTagged,
    embeddedStateGlobalEventStoreReader,
    EventMap,
    emptyEventMap,
    eventMapTVar,
    module Eventium.Store.Class,
  )
where

import Control.Concurrent.STM
import Control.Monad.State.Class
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Eventium.Store.Class
import Eventium.UUID

-- | Internal data structure used for the in-memory event stores.
data EventMap event
  = EventMap
  { _eventMapUuidMap :: Map UUID (Seq (VersionedStreamEvent event)),
    _eventMapGlobalEvents :: Seq (VersionedStreamEvent event)
  }
  deriving (Show)

-- | What it says on the tin, an initialized empty 'EventMap'
emptyEventMap :: EventMap event
emptyEventMap = EventMap Map.empty Seq.empty

-- | Initialize an 'EventMap' in a 'TVar'
eventMapTVar :: IO (TVar (EventMap event))
eventMapTVar = newTVarIO emptyEventMap

-- | An 'EventStoreReader' that stores events in a 'TVar' and runs in 'STM'.
-- This functions initializes the store by creating the 'TVar' and hooking up
-- the event store API to that 'TVar'.
tvarEventStoreReader :: TVar (EventMap event) -> VersionedEventStoreReader STM event
tvarEventStoreReader tvar = EventStoreReader $ \range -> lookupEventsInRange range <$> readTVar tvar

tvarEventStoreWriter :: TVar (EventMap event) -> VersionedEventStoreWriter STM event
tvarEventStoreWriter tvar = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion uuid = flip latestEventVersion uuid <$> readTVar tvar
    storeEvents' uuid events = do
      store <- readTVar tvar
      let (store', vers) = storeEventMap store uuid events
      writeTVar tvar store'
      return vers

-- | Like 'tvarEventStoreWriter' but accepts 'TaggedEvent's,
-- preserving the metadata attached to each event.
tvarEventStoreWriterTagged :: TVar (EventMap event) -> VersionedEventStoreWriter STM (TaggedEvent event)
tvarEventStoreWriterTagged tvar = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion uuid = flip latestEventVersion uuid <$> readTVar tvar
    storeEvents' uuid events = do
      store <- readTVar tvar
      let (store', vers) = storeEventMapTagged store uuid events
      writeTVar tvar store'
      return vers

-- | Analog of 'tvarEventStoreReader' for a 'GlobalEventStoreReader'
tvarGlobalEventStoreReader :: TVar (EventMap event) -> GlobalEventStoreReader STM event
tvarGlobalEventStoreReader tvar = EventStoreReader $ \range -> lookupGlobalEvents range <$> readTVar tvar

-- | Specialized version of 'embeddedStateEventStoreReader' that only contains an
-- 'EventMap' in the state.
stateEventStoreReader ::
  (MonadState (EventMap event) m) =>
  VersionedEventStoreReader m event
stateEventStoreReader = embeddedStateEventStoreReader id

stateGlobalEventStoreReader ::
  (MonadState (EventMap event) m) =>
  GlobalEventStoreReader m event
stateGlobalEventStoreReader = embeddedStateGlobalEventStoreReader id

-- | Specialized version of 'embeddedStateEventStoreWriter' that only contains an
-- 'EventMap' in the state.
stateEventStoreWriter ::
  (MonadState (EventMap event) m) =>
  VersionedEventStoreWriter m event
stateEventStoreWriter = embeddedStateEventStoreWriter id (const id)

-- | Like 'stateEventStoreWriter' but accepts 'TaggedEvent's.
stateEventStoreWriterTagged ::
  (MonadState (EventMap event) m) =>
  VersionedEventStoreWriter m (TaggedEvent event)
stateEventStoreWriterTagged = embeddedStateEventStoreWriterTagged id (const id)

-- | An 'EventStore' that runs on some 'MonadState' that contains an
-- 'EventMap'. This is useful if you want to include other state in your
-- 'MonadState'.
embeddedStateEventStoreReader ::
  (MonadState s m) =>
  (s -> EventMap event) ->
  VersionedEventStoreReader m event
embeddedStateEventStoreReader getMap = EventStoreReader $ \range -> lookupEventsInRange range <$> gets getMap

embeddedStateEventStoreWriter ::
  (MonadState s m) =>
  (s -> EventMap event) ->
  (s -> EventMap event -> s) ->
  VersionedEventStoreWriter m event
embeddedStateEventStoreWriter getMap setMap = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion uuid = flip latestEventVersion uuid <$> gets getMap
    storeEvents' uuid events = do
      state' <- get
      let store = getMap state'
      let (store', vers) = storeEventMap store uuid events
      put $ setMap state' store'
      return vers

-- | Analogous to 'embeddedStateEventStore' for a 'GlobalStreamEventStore'.
embeddedStateGlobalEventStoreReader ::
  (MonadState s m) =>
  (s -> EventMap event) ->
  GlobalEventStoreReader m event
embeddedStateGlobalEventStoreReader getMap = EventStoreReader $ \range -> lookupGlobalEvents range <$> gets getMap

-- | Like 'embeddedStateEventStoreWriter' but accepts 'TaggedEvent's.
embeddedStateEventStoreWriterTagged ::
  (MonadState s m) =>
  (s -> EventMap event) ->
  (s -> EventMap event -> s) ->
  VersionedEventStoreWriter m (TaggedEvent event)
embeddedStateEventStoreWriterTagged getMap setMap = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion uuid = flip latestEventVersion uuid <$> gets getMap
    storeEvents' uuid events = do
      state' <- get
      let store = getMap state'
      let (store', vers) = storeEventMapTagged store uuid events
      put $ setMap state' store'
      return vers

lookupEventMapRaw :: EventMap event -> UUID -> Seq (VersionedStreamEvent event)
lookupEventMapRaw (EventMap uuidMap _) uuid = fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsInRange :: QueryRange UUID EventVersion -> EventMap event -> [VersionedStreamEvent event]
lookupEventsInRange (QueryRange uuid start limit) store = toList $ filterEventsByRange start' limit' 0 rawEvents
  where
    start' = unEventVersion <$> start
    limit' = unEventVersion <$> limit
    rawEvents = lookupEventMapRaw store uuid

filterEventsByRange :: QueryStart Int -> QueryLimit Int -> Int -> Seq event -> Seq event
filterEventsByRange queryStart queryLimit defaultStart events =
  let (start', events') =
        case queryStart of
          StartFromBeginning -> (defaultStart, events)
          StartQueryAt start -> (start, Seq.drop (start - defaultStart) events)
      events'' =
        case queryLimit of
          NoQueryLimit -> events'
          MaxNumberOfEvents num -> Seq.take num events'
          StopQueryAt stop -> Seq.take (stop - start' + 1) events'
   in events''

latestEventVersion :: EventMap event -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupGlobalEvents :: QueryRange () SequenceNumber -> EventMap event -> [GlobalStreamEvent event]
lookupGlobalEvents (QueryRange () start limit) (EventMap _ globalEvents) = events'
  where
    start' = unSequenceNumber <$> start
    limit' = unSequenceNumber <$> limit
    events = toList $ filterEventsByRange start' limit' 1 globalEvents
    events' = zipWith (\s e -> StreamEvent () s (streamEventMetadata e) e) [startingSeqNum ..] events
    startingSeqNum =
      case start of
        StartFromBeginning -> 1
        (StartQueryAt startSeq) -> startSeq

storeEventMap ::
  EventMap event -> UUID -> [event] -> (EventMap event, EventVersion)
storeEventMap store@(EventMap uuidMap globalEvents) uuid events =
  let versStart = latestEventVersion store uuid
      streamEvents = zipWith (\v e -> StreamEvent uuid v (emptyMetadata "") e) [versStart + 1 ..] events
      newMap = Map.insertWith (flip (><)) uuid (Seq.fromList streamEvents) uuidMap
      globalEvents' = globalEvents >< Seq.fromList streamEvents
   in (EventMap newMap globalEvents', versStart + EventVersion (length events))

storeEventMapTagged ::
  EventMap event -> UUID -> [TaggedEvent event] -> (EventMap event, EventVersion)
storeEventMapTagged store@(EventMap uuidMap globalEvents) uuid taggedEvents =
  let versStart = latestEventVersion store uuid
      streamEvents =
        zipWith
          (\v (TaggedEvent meta e) -> StreamEvent uuid v meta e)
          [versStart + 1 ..]
          taggedEvents
      newMap = Map.insertWith (flip (><)) uuid (Seq.fromList streamEvents) uuidMap
      globalEvents' = globalEvents >< Seq.fromList streamEvents
   in (EventMap newMap globalEvents', versStart + EventVersion (length taggedEvents))
