{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eventium.Store.Sql.Operations
  ( SqlEventStoreConfig (..),
    sqlEventStoreReader,
    sqlGlobalEventStoreReader,
    sqlGetProjectionIds,
    sqlGetStreamEvents,
    sqlMaxEventVersion,
    sqlStoreEvents,
    sqlStoreEventsTagged,
    unsafeSqlStoreGlobalStreamEvents,
  )
where

import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Class (SafeToInsert)
import Database.Persist.Names (EntityNameDB (..), FieldNameDB (..))
import Database.Persist.Sql
import Eventium.Store.Class
import Eventium.Store.Sql.Orphans as X ()
import Eventium.UUID

data SqlEventStoreConfig entity serialized
  = SqlEventStoreConfig
  { sqlEventStoreConfigSequenceMakeEntity :: UUID -> EventVersion -> Text -> serialized -> Maybe UUID -> Maybe UUID -> Maybe UTCTime -> entity,
    -- Key manipulation
    sqlEventStoreConfigMakeKey :: SequenceNumber -> Key entity,
    sqlEventStoreConfigUnKey :: Key entity -> SequenceNumber,
    -- Record functions
    sqlEventStoreConfigUUID :: entity -> UUID,
    sqlEventStoreConfigVersion :: entity -> EventVersion,
    sqlEventStoreConfigEventType :: entity -> Text,
    sqlEventStoreConfigData :: entity -> serialized,
    sqlEventStoreConfigCorrelationId :: entity -> Maybe UUID,
    sqlEventStoreConfigCausationId :: entity -> Maybe UUID,
    sqlEventStoreConfigCreatedAt :: entity -> Maybe UTCTime,
    -- EntityFields
    sqlEventStoreConfigSequenceNumberField :: EntityField entity (Key entity),
    sqlEventStoreConfigUUIDField :: EntityField entity UUID,
    sqlEventStoreConfigVersionField :: EntityField entity EventVersion,
    sqlEventStoreConfigDataField :: EntityField entity serialized
  }

sqlEventStoreReader ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  VersionedEventStoreReader (SqlPersistT m) serialized
sqlEventStoreReader config = EventStoreReader $ sqlGetStreamEvents config

sqlGlobalEventStoreReader ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  GlobalEventStoreReader (SqlPersistT m) serialized
sqlGlobalEventStoreReader config =
  EventStoreReader $ sqlGetAllEventsInRange config

sqlEventToGlobalStream ::
  SqlEventStoreConfig entity serialized ->
  Entity entity ->
  GlobalStreamEvent serialized
sqlEventToGlobalStream config@SqlEventStoreConfig {..} (Entity key event) =
  let versioned = sqlEventToVersioned config event
   in StreamEvent () (sqlEventStoreConfigUnKey key) (streamEventMetadata versioned) versioned

sqlEventToVersioned ::
  SqlEventStoreConfig entity serialized ->
  entity ->
  VersionedStreamEvent serialized
sqlEventToVersioned SqlEventStoreConfig {..} entity =
  StreamEvent
    (sqlEventStoreConfigUUID entity)
    (sqlEventStoreConfigVersion entity)
    ( EventMetadata
        (sqlEventStoreConfigEventType entity)
        (sqlEventStoreConfigCorrelationId entity)
        (sqlEventStoreConfigCausationId entity)
        (sqlEventStoreConfigCreatedAt entity)
    )
    (sqlEventStoreConfigData entity)

sqlGetProjectionIds ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  SqlPersistT m [UUID]
sqlGetProjectionIds SqlEventStoreConfig {..} =
  fmap unSingle <$> rawSql ("SELECT DISTINCT " <> uuidFieldName <> " FROM " <> tableName) []
  where
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)
    uuidFieldName = unFieldNameDB $ fieldDBName sqlEventStoreConfigUUIDField

sqlGetStreamEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  QueryRange UUID EventVersion ->
  SqlPersistT m [VersionedStreamEvent serialized]
sqlGetStreamEvents config@SqlEventStoreConfig {..} QueryRange {..} = do
  entities <- selectList filters selectOpts
  return $ sqlEventToVersioned config . entityVal <$> entities
  where
    startFilter =
      case queryRangeStart of
        StartFromBeginning -> []
        StartQueryAt start -> [sqlEventStoreConfigVersionField >=. start]
    (endFilter, endSelectOpt) =
      case queryRangeLimit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([sqlEventStoreConfigVersionField <=. stop], [])
    filters = (sqlEventStoreConfigUUIDField ==. queryRangeKey) : startFilter ++ endFilter
    selectOpts = Asc sqlEventStoreConfigSequenceNumberField : endSelectOpt

sqlGetAllEventsInRange ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  QueryRange () SequenceNumber ->
  SqlPersistT m [GlobalStreamEvent serialized]
sqlGetAllEventsInRange config@SqlEventStoreConfig {..} QueryRange {..} = do
  entities <- selectList filters selectOpts
  return $ sqlEventToGlobalStream config <$> entities
  where
    startFilter =
      case queryRangeStart of
        StartFromBeginning -> []
        StartQueryAt start -> [sqlEventStoreConfigSequenceNumberField >=. sqlEventStoreConfigMakeKey start]
    (endFilter, endSelectOpt) =
      case queryRangeLimit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([sqlEventStoreConfigSequenceNumberField <=. sqlEventStoreConfigMakeKey stop], [])
    filters = startFilter ++ endFilter
    selectOpts = Asc sqlEventStoreConfigSequenceNumberField : endSelectOpt

sqlMaxEventVersion ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  (FieldNameDB -> FieldNameDB -> FieldNameDB -> Text) ->
  UUID ->
  SqlPersistT m EventVersion
sqlMaxEventVersion SqlEventStoreConfig {..} maxVersionSql uuid =
  let tableName = FieldNameDB $ unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)
      uuidFieldName = fieldDBName sqlEventStoreConfigUUIDField
      versionFieldName = fieldDBName sqlEventStoreConfigVersionField
      rawVals = rawSql (maxVersionSql tableName uuidFieldName versionFieldName) [toPersistValue uuid]
   in maybe 0 unSingle . listToMaybe <$> rawVals

sqlStoreEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  Maybe (Text -> Text) ->
  (FieldNameDB -> FieldNameDB -> FieldNameDB -> Text) ->
  UUID ->
  [serialized] ->
  SqlPersistT m EventVersion
sqlStoreEvents config@SqlEventStoreConfig {..} mLockCommand maxVersionSql uuid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities = zipWith (\v e -> sqlEventStoreConfigSequenceMakeEntity uuid v "" e Nothing Nothing Nothing) [versionNum + 1 ..] events
  -- NB: We need to take a lock on the events table or else the global sequence
  -- numbers may not increase monotonically over time.
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length events)
  where
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)

-- | Like 'sqlStoreEvents' but accepts 'TaggedEvent's, using their metadata
-- for event type name, correlation ID, causation ID, and timestamp.
sqlStoreEventsTagged ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  Maybe (Text -> Text) ->
  (FieldNameDB -> FieldNameDB -> FieldNameDB -> Text) ->
  UUID ->
  [TaggedEvent serialized] ->
  SqlPersistT m EventVersion
sqlStoreEventsTagged config@SqlEventStoreConfig {..} mLockCommand maxVersionSql uuid taggedEvents = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities =
        zipWith
          ( \v (TaggedEvent meta e) ->
              sqlEventStoreConfigSequenceMakeEntity
                uuid
                v
                (eventMetadataEventType meta)
                e
                (eventMetadataCorrelationId meta)
                (eventMetadataCausationId meta)
                (eventMetadataCreatedAt meta)
          )
          [versionNum + 1 ..]
          taggedEvents
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length taggedEvents)
  where
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)

-- | Useful if you have some 'GlobalStreamEvent's and you want to shove them in
-- a SQL event store. This can happen when you are moving events between event
-- stores, or you somehow generate the events outside of the current SQL event
-- store.
unsafeSqlStoreGlobalStreamEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  [GlobalStreamEvent serialized] ->
  SqlPersistT m ()
unsafeSqlStoreGlobalStreamEvents SqlEventStoreConfig {..} events =
  insertEntityMany $ fmap mkEventEntity events
  where
    mkEventEntity (StreamEvent () seqNum _ (StreamEvent uuid vers meta event)) =
      Entity
        (sqlEventStoreConfigMakeKey seqNum)
        ( sqlEventStoreConfigSequenceMakeEntity
            uuid
            vers
            (eventMetadataEventType meta)
            event
            (eventMetadataCorrelationId meta)
            (eventMetadataCausationId meta)
            (eventMetadataCreatedAt meta)
        )
