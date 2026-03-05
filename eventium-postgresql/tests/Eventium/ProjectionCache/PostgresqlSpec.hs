{-# LANGUAGE OverloadedStrings #-}

module Eventium.ProjectionCache.PostgresqlSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Database.Persist.Postgresql
import Eventium.ProjectionCache.Cache (codecProjectionCache)
import Eventium.ProjectionCache.Postgresql
import Eventium.Store.Postgresql
import Eventium.Testkit
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "PostgreSQL versioned projection cache" $ do
    versionedProjectionCacheSpec postgresVersionedProjectionCacheRunner

  describe "PostgreSQL global projection cache" $ do
    globalProjectionCacheSpec postgresGlobalProjectionCacheRunner

makePool :: IO ConnectionPool
makePool = do
  let makeConnString host port user pass db =
        "host="
          <> host
          <> " port="
          <> port
          <> " user="
          <> user
          <> " dbname="
          <> db
          <> " password="
          <> pass
  connString <-
    makeConnString
      <$> getEnvDef "POSTGRES_HOST" "127.0.0.1"
      <*> getEnvDef "POSTGRES_PORT" "5432"
      <*> getEnvDef "POSTGRES_USER" "postgres"
      <*> getEnvDef "POSTGRES_PASSWORD" "password"
      <*> getEnvDef "POSTGRES_DBNAME" "eventium_test"
  pool <- runNoLoggingT (createPostgresqlPool connString 1)
  flip runSqlPool pool $ do
    void $ runMigrationSilent migrateSqlEvent
    void $ runMigrationSilent migrateProjectionSnapshot
    truncateTables
  return pool

getEnvDef :: String -> ByteString -> IO ByteString
getEnvDef name def = maybe def UTF8.fromString <$> lookupEnv name

truncateTables :: (MonadIO m) => SqlPersistT m ()
truncateTables = do
  rawExecute "TRUNCATE TABLE events RESTART IDENTITY" []
  rawExecute "TRUNCATE TABLE projection_snapshots" []

postgresVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner (SqlPersistT IO)
postgresVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> do
  pool <- makePool
  let writer =
        codecEventStoreWriter jsonStringCodec $
          postgresqlEventStoreWriter defaultSqlEventStoreConfig
      reader =
        codecVersionedEventStoreReader jsonStringCodec $
          sqlEventStoreReader defaultSqlEventStoreConfig
      cache =
        codecProjectionCache jsonStringCodec $
          postgresqlVersionedProjectionCache (ProjectionName "test_versioned")
  runSqlPool (action writer reader cache) pool

postgresGlobalProjectionCacheRunner :: GlobalProjectionCacheRunner (SqlPersistT IO)
postgresGlobalProjectionCacheRunner = GlobalProjectionCacheRunner $ \action -> do
  pool <- makePool
  let writer =
        codecEventStoreWriter jsonStringCodec $
          postgresqlEventStoreWriter defaultSqlEventStoreConfig
      globalReader =
        codecGlobalEventStoreReader jsonStringCodec $
          sqlGlobalEventStoreReader defaultSqlEventStoreConfig
      cache =
        codecProjectionCache jsonStringCodec $
          postgresqlGlobalProjectionCache (ProjectionName "test_global")
  runSqlPool (action writer globalReader cache) pool
