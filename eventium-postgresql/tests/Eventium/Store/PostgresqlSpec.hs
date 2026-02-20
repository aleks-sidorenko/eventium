{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.PostgresqlSpec (spec) where

import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (maybe)
import Data.Text (Text)
import Database.Persist.Postgresql
import Eventium.Store.Postgresql
import Eventium.Testkit
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = do
  describe "Postgres event store" $ do
    eventStoreSpec postgresStoreRunner
    globalStreamEventStoreSpec postgresStoreGlobalRunner

makeStore ::
  (MonadIO m) =>
  m
    ( VersionedEventStoreWriter (SqlPersistT m) CounterEvent,
      VersionedEventStoreReader (SqlPersistT m) CounterEvent,
      ConnectionPool
    )
makeStore = do
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
      writer =
        codecEventStoreWriter jsonStringCodec $
          postgresqlEventStoreWriter defaultSqlEventStoreConfig
      reader =
        codecVersionedEventStoreReader jsonStringCodec $
          sqlEventStoreReader defaultSqlEventStoreConfig
  connString <-
    makeConnString
      <$> getEnvDef "POSTGRES_HOST" "localhost"
      <*> getEnvDef "POSTGRES_PORT" "5432"
      <*> getEnvDef "POSTGRES_USER" "postgres"
      <*> getEnvDef "POSTGRES_PASSWORD" "password"
      <*> getEnvDef "POSTGRES_DBNAME" "eventium_test"
  pool <- liftIO $ runNoLoggingT (createPostgresqlPool connString 1)
  liftIO $ flip runSqlPool pool $ do
    void $ runMigrationSilent migrateSqlEvent
    truncateTables
  return (writer, reader, pool)

getEnvDef :: (MonadIO m) => String -> ByteString -> m ByteString
getEnvDef name def = liftIO $ maybe def UTF8.fromString <$> lookupEnv name

truncateTables :: (MonadIO m) => SqlPersistT m ()
truncateTables = do
  -- Ensure both rows and the sequence are reset between tests
  rawExecute "TRUNCATE TABLE events RESTART IDENTITY" []

postgresStoreRunner :: EventStoreRunner (SqlPersistT IO)
postgresStoreRunner = EventStoreRunner $ \action -> do
  (writer, reader, pool) <- makeStore
  runSqlPool (action writer reader) pool

postgresStoreGlobalRunner :: GlobalStreamEventStoreRunner (SqlPersistT IO)
postgresStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (writer, _, pool) <- makeStore
  let globalReader = codecGlobalEventStoreReader jsonStringCodec (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)
  runSqlPool (action writer globalReader) pool
