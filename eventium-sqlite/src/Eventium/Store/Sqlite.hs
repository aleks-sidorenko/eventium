{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines an Sqlite event store.
module Eventium.Store.Sqlite
  ( sqliteEventStoreWriter,
    sqliteEventStoreWriterTagged,
    initializeSqliteEventStore,
    module Eventium.Store.Class,
    module Eventium.Store.Sql,
  )
where

import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist
import Database.Persist.Class (SafeToInsert)
import Database.Persist.Names (EntityNameDB (..), FieldNameDB (..))
import Database.Persist.Sql
import Eventium.Store.Class
import Eventium.Store.Sql

-- | An 'EventStoreWriter' that uses an SQLite database as a backend. Use
-- 'SqlEventStoreConfig' to configure this event store.
sqliteEventStoreWriter ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  VersionedEventStoreWriter (SqlPersistT m) serialized
sqliteEventStoreWriter config = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion = sqlMaxEventVersion config maxSqliteVersionSql
    storeEvents' = sqlStoreEvents config Nothing maxSqliteVersionSql

-- | Like 'sqliteEventStoreWriter' but accepts 'TaggedEvent's,
-- preserving the metadata attached to each event.
sqliteEventStoreWriterTagged ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  VersionedEventStoreWriter (SqlPersistT m) (TaggedEvent serialized)
sqliteEventStoreWriterTagged config = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion = sqlMaxEventVersion config maxSqliteVersionSql
    storeEvents' = sqlStoreEventsTagged config Nothing maxSqliteVersionSql

maxSqliteVersionSql :: FieldNameDB -> FieldNameDB -> FieldNameDB -> Text
maxSqliteVersionSql (FieldNameDB tableName) (FieldNameDB uuidFieldName) (FieldNameDB versionFieldName) =
  "SELECT IFNULL(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

-- | This functions runs the migrations required to create the events table and
-- also adds an index on the UUID column.
initializeSqliteEventStore ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  ConnectionPool ->
  m ()
initializeSqliteEventStore SqlEventStoreConfig {..} pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  -- Create index on uuid field so retrieval is very fast
  let tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity undefined undefined undefined undefined undefined undefined undefined)
      uuidFieldName = unFieldNameDB $ fieldDBName sqlEventStoreConfigSequenceNumberField
      indexSql =
        "CREATE INDEX IF NOT EXISTS "
          <> uuidFieldName
          <> "_index"
          <> " ON "
          <> tableName
          <> " ("
          <> uuidFieldName
          <> ")"
  liftIO $ flip runSqlPool pool $ rawExecute indexSql []

  return ()
