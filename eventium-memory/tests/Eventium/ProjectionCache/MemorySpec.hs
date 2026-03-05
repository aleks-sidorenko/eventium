module Eventium.ProjectionCache.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Eventium.ProjectionCache.Memory
import Eventium.Store.Memory
import Eventium.Testkit
import MemoryTestImport
import Test.Hspec

spec :: Spec
spec = do
  describe "TVar projection cache" $ do
    versionedProjectionCacheSpec tvarVersionedProjectionCacheRunner
    globalProjectionCacheSpec tvarGlobalProjectionCacheRunner

  describe "MonadState embedded memory projection cache" $ do
    versionedProjectionCacheSpec stateVersionedProjectionCacheRunner
    globalProjectionCacheSpec stateGlobalProjectionCacheRunner

tvarVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner STM
tvarVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let writer = tvarEventStoreWriter eventTVar
      reader = tvarEventStoreReader eventTVar
      cache = tvarProjectionCache projTVar
  atomically $ action writer reader cache

tvarGlobalProjectionCacheRunner :: GlobalProjectionCacheRunner STM
tvarGlobalProjectionCacheRunner = GlobalProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let writer = tvarEventStoreWriter eventTVar
      globalReader = tvarGlobalEventStoreReader eventTVar
      cache = tvarProjectionCache projTVar
  atomically $ action writer globalReader cache

stateVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
stateVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> evalStateT (action writer reader cache) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter (.eventMap) setEventMap
    reader = embeddedStateEventStoreReader (.eventMap)
    cache = embeddedStateProjectionCache (.projectionMap) setProjectionMap

stateGlobalProjectionCacheRunner :: GlobalProjectionCacheRunner (StateT (GlobalEmbeddedState Counter CounterEvent) IO)
stateGlobalProjectionCacheRunner =
  GlobalProjectionCacheRunner $ \action -> evalStateT (action writer globalReader cache) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter (.eventMap) setEventMap
    globalReader = embeddedStateGlobalEventStoreReader (.eventMap)
    cache = embeddedStateProjectionCache (.projectionMap) setProjectionMap
