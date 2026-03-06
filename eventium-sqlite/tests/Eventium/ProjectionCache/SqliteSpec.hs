{-# LANGUAGE OverloadedStrings #-}

module Eventium.ProjectionCache.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Eventium.ProjectionCache.Cache (codecProjectionCache)
import Eventium.ProjectionCache.Sqlite
import Eventium.Store.Sqlite
import Eventium.Testkit
import Test.Hspec

spec :: Spec
spec = do
  describe "SQLite versioned projection cache" $ do
    versionedProjectionCacheSpec sqliteVersionedProjectionCacheRunner

  describe "SQLite global projection cache" $ do
    globalProjectionCacheSpec sqliteGlobalProjectionCacheRunner

  describe "SQLite checkpoint store" $ do
    checkpointStoreSpec sqliteCheckpointStoreRunner

makeStore ::
  IO
    ( VersionedEventStoreWriter (SqlPersistT IO) CounterEvent,
      VersionedEventStoreReader (SqlPersistT IO) CounterEvent,
      ConnectionPool
    )
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  let writer = codecEventStoreWriter jsonStringCodec $ sqliteEventStoreWriter defaultSqlEventStoreConfig
      reader = codecVersionedEventStoreReader jsonStringCodec $ sqlEventStoreReader defaultSqlEventStoreConfig
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  initializeSqliteProjectionCache pool
  return (writer, reader, pool)

sqliteVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner (SqlPersistT IO)
sqliteVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> do
  (writer, reader, pool) <- makeStore
  let cache =
        codecProjectionCache jsonStringCodec $
          sqliteVersionedProjectionCache (ProjectionName "test_versioned")
  runSqlPool (action writer reader cache) pool

sqliteGlobalProjectionCacheRunner :: GlobalProjectionCacheRunner (SqlPersistT IO)
sqliteGlobalProjectionCacheRunner = GlobalProjectionCacheRunner $ \action -> do
  (writer, _, pool) <- makeStore
  let globalReader =
        codecGlobalEventStoreReader jsonStringCodec $
          sqlGlobalEventStoreReader defaultSqlEventStoreConfig
      cache =
        codecProjectionCache jsonStringCodec $
          sqliteGlobalProjectionCache (ProjectionName "test_global")
  runSqlPool (action writer globalReader cache) pool

sqliteCheckpointStoreRunner :: CheckpointStoreRunner (SqlPersistT IO)
sqliteCheckpointStoreRunner = CheckpointStoreRunner $ \action -> do
  (_, _, pool) <- makeStore
  runSqlPool (action (sqliteCheckpointStore (CheckpointName "test_checkpoint"))) pool
