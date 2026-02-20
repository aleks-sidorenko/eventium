{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines an Postgresql event store.
module Eventium.Store.Postgresql
  ( postgresqlEventStoreWriter,
    postgresqlEventStoreWriterTagged,
    module Eventium.Store.Class,
    module Eventium.Store.Sql,
  )
where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Names (EntityNameDB (..), FieldNameDB (..))
import Database.Persist.Sql
import Eventium.Store.Class
import Eventium.Store.Sql

-- | An 'EventStore' that uses a PostgreSQL database as a backend. Use
-- 'SqlEventStoreConfig' to configure this event store.
postgresqlEventStoreWriter ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  VersionedEventStoreWriter (SqlPersistT m) serialized
postgresqlEventStoreWriter config = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion = sqlMaxEventVersion config maxPostgresVersionSql
    storeEvents' = sqlStoreEvents config (Just tableLockFunc) maxPostgresVersionSql

maxPostgresVersionSql :: FieldNameDB -> FieldNameDB -> FieldNameDB -> Text
maxPostgresVersionSql (FieldNameDB tableName) (FieldNameDB uuidFieldName) (FieldNameDB versionFieldName) =
  "SELECT COALESCE(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

-- | Like 'postgresqlEventStoreWriter' but accepts 'TaggedEvent's,
-- preserving the metadata attached to each event.
postgresqlEventStoreWriterTagged ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  VersionedEventStoreWriter (SqlPersistT m) (TaggedEvent serialized)
postgresqlEventStoreWriterTagged config = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion = sqlMaxEventVersion config maxPostgresVersionSql
    storeEvents' = sqlStoreEventsTagged config (Just tableLockFunc) maxPostgresVersionSql

-- | We need to lock the events table or else our global sequence number might
-- not be monotonically increasing over time from the point of view of a
-- reader.
--
-- For example, say transaction A begins to write an event and the
-- auto-increment key is 1. Then, transaction B starts to insert an event and
-- gets an id of 2. If transaction B is quick and completes, then a listener
-- might see the event from B and thinks it has all the events up to a sequence
-- number of 2. However, once A finishes and the event with the id of 1 is
-- done, then the listener won't know that event exists.
tableLockFunc :: Text -> Text
tableLockFunc tableName = "LOCK " <> tableName <> " IN EXCLUSIVE MODE"
