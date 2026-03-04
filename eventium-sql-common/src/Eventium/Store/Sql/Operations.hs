{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Eventium.Store.Class
import Eventium.Store.Sql.JSONString (JSONString, decodeJSON, encodeJSON)
import Eventium.Store.Sql.Orphans as X ()
import Eventium.UUID

data SqlEventStoreConfig entity serialized
  = SqlEventStoreConfig
  { sequenceMakeEntity :: UUID -> EventVersion -> serialized -> Maybe JSONString -> entity,
    -- Key manipulation
    makeKey :: SequenceNumber -> Key entity,
    unKey :: Key entity -> SequenceNumber,
    -- Record functions
    uuid :: entity -> UUID,
    version :: entity -> EventVersion,
    payload :: entity -> serialized,
    metadata :: entity -> Maybe JSONString,
    -- EntityFields
    sequenceNumberField :: EntityField entity (Key entity),
    uuidField :: EntityField entity UUID,
    versionField :: EntityField entity EventVersion,
    payloadField :: EntityField entity serialized,
    metadataField :: EntityField entity (Maybe JSONString)
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
sqlEventToGlobalStream config (Entity eKey event) =
  let versioned = sqlEventToVersioned config event
   in StreamEvent () (config.unKey eKey) versioned.metadata versioned

-- | Decode metadata from a stored JSONString. Returns empty metadata on NULL
-- or decode failure, ensuring events are always readable.
decodeMetadata :: Maybe JSONString -> EventMetadata
decodeMetadata = fromMaybe (emptyMetadata "") . (>>= decodeJSON)

sqlEventToVersioned ::
  SqlEventStoreConfig entity serialized ->
  entity ->
  VersionedStreamEvent serialized
sqlEventToVersioned config entity =
  StreamEvent
    (config.uuid entity)
    (config.version entity)
    (decodeMetadata $ config.metadata entity)
    (config.payload entity)

sqlGetProjectionIds ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  SqlPersistT m [UUID]
sqlGetProjectionIds config =
  fmap unSingle <$> rawSql ("SELECT DISTINCT " <> uuidFieldName <> " FROM " <> tableName) []
  where
    tableName = unEntityNameDB $ tableDBName (config.sequenceMakeEntity nil 0 undefined Nothing)
    uuidFieldName = unFieldNameDB $ fieldDBName config.uuidField

sqlGetStreamEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  QueryRange UUID EventVersion ->
  SqlPersistT m [VersionedStreamEvent serialized]
sqlGetStreamEvents config query = do
  entities <- selectList filters selectOpts
  return $ sqlEventToVersioned config . entityVal <$> entities
  where
    startFilter =
      case query.start of
        StartFromBeginning -> []
        StartQueryAt s -> [config.versionField >=. s]
    (endFilter, endSelectOpt) =
      case query.limit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([config.versionField <=. stop], [])
    filters = (config.uuidField ==. query.key) : startFilter ++ endFilter
    selectOpts = Asc config.sequenceNumberField : endSelectOpt

sqlGetAllEventsInRange ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  QueryRange () SequenceNumber ->
  SqlPersistT m [GlobalStreamEvent serialized]
sqlGetAllEventsInRange config query = do
  entities <- selectList filters selectOpts
  return $ sqlEventToGlobalStream config <$> entities
  where
    startFilter =
      case query.start of
        StartFromBeginning -> []
        StartQueryAt s -> [config.sequenceNumberField >=. config.makeKey s]
    (endFilter, endSelectOpt) =
      case query.limit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([config.sequenceNumberField <=. config.makeKey stop], [])
    filters = startFilter ++ endFilter
    selectOpts = Asc config.sequenceNumberField : endSelectOpt

sqlMaxEventVersion ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  (FieldNameDB -> FieldNameDB -> FieldNameDB -> Text) ->
  UUID ->
  SqlPersistT m EventVersion
sqlMaxEventVersion config maxVersionSql uid =
  let tableName = FieldNameDB $ unEntityNameDB $ tableDBName (config.sequenceMakeEntity nil 0 undefined Nothing)
      uuidFieldName = fieldDBName config.uuidField
      versionFieldName = fieldDBName config.versionField
      rawVals = rawSql (maxVersionSql tableName uuidFieldName versionFieldName) [toPersistValue uid]
   in maybe 0 unSingle . listToMaybe <$> rawVals

sqlStoreEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend, SafeToInsert entity) =>
  SqlEventStoreConfig entity serialized ->
  Maybe (Text -> Text) ->
  (FieldNameDB -> FieldNameDB -> FieldNameDB -> Text) ->
  UUID ->
  [serialized] ->
  SqlPersistT m EventVersion
sqlStoreEvents config mLockCommand maxVersionSql uid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uid
  let entities = zipWith (\v e -> config.sequenceMakeEntity uid v e Nothing) [versionNum + 1 ..] events
  -- NB: We need to take a lock on the events table or else the global sequence
  -- numbers may not increase monotonically over time.
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length events)
  where
    tableName = unEntityNameDB $ tableDBName (config.sequenceMakeEntity nil 0 undefined Nothing)

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
sqlStoreEventsTagged config mLockCommand maxVersionSql uid taggedEvents = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uid
  let entities =
        zipWith
          ( \v (TaggedEvent meta e) ->
              config.sequenceMakeEntity
                uid
                v
                e
                (Just $ encodeJSON meta)
          )
          [versionNum + 1 ..]
          taggedEvents
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length taggedEvents)
  where
    tableName = unEntityNameDB $ tableDBName (config.sequenceMakeEntity nil 0 undefined Nothing)

-- | Useful if you have some 'GlobalStreamEvent's and you want to shove them in
-- a SQL event store. This can happen when you are moving events between event
-- stores, or you somehow generate the events outside of the current SQL event
-- store.
unsafeSqlStoreGlobalStreamEvents ::
  (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend) =>
  SqlEventStoreConfig entity serialized ->
  [GlobalStreamEvent serialized] ->
  SqlPersistT m ()
unsafeSqlStoreGlobalStreamEvents config events =
  insertEntityMany $ fmap mkEventEntity events
  where
    mkEventEntity (StreamEvent () seqNum _ (StreamEvent uid vers meta evtPayload)) =
      Entity
        (config.makeKey seqNum)
        ( config.sequenceMakeEntity
            uid
            vers
            evtPayload
            (Just $ encodeJSON meta)
        )
