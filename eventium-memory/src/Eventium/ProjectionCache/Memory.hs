{-# LANGUAGE FlexibleContexts #-}

module Eventium.ProjectionCache.Memory
  ( ProjectionMap,
    emptyProjectionMap,
    projectionMapTVar,
    tvarProjectionCache,
    embeddedStateProjectionCache,
    module Eventium.ProjectionCache.Cache,
  )
where

import Control.Concurrent.STM
import Control.Monad.State.Class hiding (state)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Eventium.ProjectionCache.Cache

-- | A 'ProjectionMap' just stores the latest snapshot for each UUID.
type ProjectionMap key position encoded = Map key (position, encoded)

emptyProjectionMap :: ProjectionMap key position encoded
emptyProjectionMap = Map.empty

projectionMapTVar :: IO (TVar (ProjectionMap key position encoded))
projectionMapTVar = newTVarIO emptyProjectionMap

storeProjectionInMap ::
  (Ord key) =>
  key ->
  position ->
  encoded ->
  ProjectionMap key position encoded ->
  ProjectionMap key position encoded
storeProjectionInMap uuid version state = Map.insert uuid (version, state)

-- | A 'ProjectionCache' that uses a 'TVar' and runs in 'STM'.
tvarProjectionCache ::
  (Ord key) =>
  TVar (ProjectionMap key position encoded) ->
  ProjectionCache key position encoded STM
tvarProjectionCache tvar =
  ProjectionCache
    { storeSnapshot = \uuid version projState -> modifyTVar' tvar (storeProjectionInMap uuid version projState),
      loadSnapshot = \uuid -> Map.lookup uuid <$> readTVar tvar
    }

-- | A 'ProjectionCache' for some 'MonadState' that contains a 'ProjectionMap'.
embeddedStateProjectionCache ::
  (MonadState s m, Ord key) =>
  (s -> ProjectionMap key position encoded) ->
  (s -> ProjectionMap key position encoded -> s) ->
  ProjectionCache key position encoded m
embeddedStateProjectionCache getMap setMap =
  ProjectionCache
    { storeSnapshot = \uuid version projState -> modify' (storeSnapshot uuid version projState),
      loadSnapshot = \uuid -> Map.lookup uuid <$> gets getMap
    }
  where
    storeSnapshot uuid version projState st =
      setMap st $ storeProjectionInMap uuid version projState $ getMap st
