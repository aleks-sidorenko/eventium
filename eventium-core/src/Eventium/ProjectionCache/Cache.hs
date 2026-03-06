{-# LANGUAGE RankNTypes #-}

module Eventium.ProjectionCache.Cache
  ( runProjectionCacheUsing,
    codecProjectionCache,
    getLatestVersionedProjectionWithCache,
    getLatestGlobalProjectionWithCache,
    updateVersionedProjectionCache,
    updateGlobalProjectionCache,
    snapshotEventHandler,
    snapshotGlobalEventHandler,
    module Eventium.ProjectionCache.Types,
  )
where

import Eventium.Codec
import Eventium.EventHandler
import Eventium.Projection
import Eventium.ProjectionCache.Types
import Eventium.Store.Class

-- | Changes the monad a 'ProjectionCache' runs in. This is useful to run the
-- cache in another 'Monad' while forgetting the original 'Monad'.
runProjectionCacheUsing ::
  (forall a. mstore a -> m a) ->
  ProjectionCache key position encoded mstore ->
  ProjectionCache key position encoded m
runProjectionCacheUsing runCache pc =
  ProjectionCache
    { storeSnapshot = \uuid version st -> runCache $ pc.storeSnapshot uuid version st,
      loadSnapshot = runCache . pc.loadSnapshot
    }

-- | Wraps a 'ProjectionCache' and transparently encodes/decodes events for
-- you. Note that in this implementation decoding errors when using
-- 'getEvents' are simply ignored (the event is not returned).
codecProjectionCache ::
  (Monad m) =>
  Codec state encoded ->
  ProjectionCache key position encoded m ->
  ProjectionCache key position state m
codecProjectionCache codec pc =
  ProjectionCache storeSnapshot' loadSnapshot'
  where
    storeSnapshot' uuid version = pc.storeSnapshot uuid version . codec.encode
    loadSnapshot' uuid = do
      mState <- pc.loadSnapshot uuid
      return $ mState >>= traverse codec.decode

-- | Like 'getLatestVersionedProjection', but uses a 'ProjectionCache' if it contains
-- more recent state.
getLatestVersionedProjectionWithCache ::
  (Monad m) =>
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  VersionedStreamProjection state event ->
  m (VersionedStreamProjection state event)
getLatestVersionedProjectionWithCache store cache proj =
  getLatestProjectionWithCache' cache proj proj.key >>= getLatestStreamProjection store

-- | Like 'getLatestGlobalProjection', but uses a 'ProjectionCache' if it
-- contains more recent state.
getLatestGlobalProjectionWithCache ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  GlobalProjectionCache state m ->
  GlobalStreamProjection state event ->
  m (GlobalStreamProjection state event)
getLatestGlobalProjectionWithCache store cache proj =
  getLatestProjectionWithCache' cache proj () >>= getLatestStreamProjection store

getLatestProjectionWithCache' ::
  (Monad m, Ord position) =>
  ProjectionCache key position state m ->
  StreamProjection projKey position state event ->
  key ->
  m (StreamProjection projKey position state event)
getLatestProjectionWithCache' cache proj k = do
  mLatestState <- cache.loadSnapshot k
  let mkProjection' (pos, st) =
        if pos > proj.position
          then
            proj
              { position = pos,
                state = st
              }
          else proj
  return $ maybe proj mkProjection' mLatestState

-- | Loads the latest projection state from the cache/store and stores this
-- value back into the projection cache.
updateVersionedProjectionCache ::
  (Monad m) =>
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  VersionedStreamProjection state event ->
  m ()
updateVersionedProjectionCache reader cache proj = do
  sp <- getLatestVersionedProjectionWithCache reader cache proj
  cache.storeSnapshot sp.key sp.position sp.state

-- | Analog of 'updateVersionedProjectionCache' for a 'GlobalProjectionCache'.
updateGlobalProjectionCache ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  GlobalProjectionCache state m ->
  GlobalStreamProjection state event ->
  m ()
updateGlobalProjectionCache reader cache proj = do
  sp <- getLatestGlobalProjectionWithCache reader cache proj
  cache.storeSnapshot () sp.position sp.state

-- | Creates an 'EventHandler' that updates a 'VersionedProjectionCache'
-- whenever a versioned stream event is received. Compose with
-- @synchronousPublisher@ or @publishingEventStoreWriter@ to keep the cache
-- up to date as events are written.
snapshotEventHandler ::
  (Monad m) =>
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  Projection state event ->
  EventHandler m (VersionedStreamEvent event)
snapshotEventHandler reader cache proj =
  EventHandler $ \streamEvent -> do
    let uuid = streamEvent.key
    updateVersionedProjectionCache reader cache (versionedStreamProjection uuid proj)

-- | Creates an 'EventHandler' that updates a 'GlobalProjectionCache'
-- whenever a global stream event is received. Compose with
-- @synchronousPublisher@ or @publishingEventStoreWriter@ to keep the cache
-- up to date as events are written.
snapshotGlobalEventHandler ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  GlobalProjectionCache state m ->
  Projection state (VersionedStreamEvent event) ->
  EventHandler m (GlobalStreamEvent event)
snapshotGlobalEventHandler reader cache proj =
  EventHandler $ \_ ->
    updateGlobalProjectionCache reader cache (globalStreamProjection proj)
