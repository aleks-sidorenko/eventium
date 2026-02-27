# Metadata as JSONB Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace individual metadata columns in the SQL event store with a single `metadata jsonb NULL` column.

**Architecture:** The `EventMetadata` record in `eventium-core` gains `ToJSON`/`FromJSON` instances (via `DeriveGeneric`). The Persistent entity `SqlEvent` drops 4 metadata columns and adds one `metadata JSONString Maybe` column. `SqlEventStoreConfig` is simplified to use a single metadata accessor. Writers encode `EventMetadata` to JSONB; readers decode it back (defaulting to empty metadata on NULL).

**Tech Stack:** Haskell, Persistent ORM, Aeson, hspec

---

### Task 1: Add ToJSON/FromJSON to EventMetadata

**Files:**
- Modify: `eventium-core/src/Eventium/Store/Types.hs:1-47`

**Step 1: Add DeriveGeneric pragma and Generic import**

Add `DeriveGeneric` to the language pragmas at line 2, and add `GHC.Generics (Generic)` to imports.

Current file starts with:
```haskell
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

Change to:
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

Add import:
```haskell
import GHC.Generics (Generic)
```

**Step 2: Add Generic deriving and JSON instances to EventMetadata**

Change:
```haskell
data EventMetadata = EventMetadata
  { eventMetadataEventType :: !Text,
    eventMetadataCorrelationId :: !(Maybe UUID),
    eventMetadataCausationId :: !(Maybe UUID),
    eventMetadataCreatedAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq)
```

To:
```haskell
data EventMetadata = EventMetadata
  { eventMetadataEventType :: !Text,
    eventMetadataCorrelationId :: !(Maybe UUID),
    eventMetadataCausationId :: !(Maybe UUID),
    eventMetadataCreatedAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

instance ToJSON EventMetadata

instance FromJSON EventMetadata
```

Note: `Data.Aeson` (`ToJSON`, `FromJSON`) is already imported at line 29. `UUID` already has `ToJSON`/`FromJSON` from the `uuid-aeson` or `Data.UUID.Aeson` instances (check that `uuid` package provides these, or that orphan instances exist).

**Step 3: Build eventium-core to verify**

Run: `cabal build eventium-core`
Expected: SUCCESS. If UUID lacks `ToJSON`/`FromJSON`, we may need to add orphan instances or a dependency. Handle if needed.

**Step 4: Run eventium-core tests**

Run: `cabal test eventium-core`
Expected: PASS

**Step 5: Commit**

```bash
git add eventium-core/src/Eventium/Store/Types.hs
git commit -m "feat(eventium-core): add ToJSON/FromJSON instances to EventMetadata"
```

---

### Task 2: Export encodeJSON/decodeJSON from JSONString module

**Files:**
- Modify: `eventium-sql-common/src/Eventium/Store/Sql/JSONString.hs:4-7`

The `Operations.hs` module will need `encodeJSON` and `decodeJSON` to serialize/deserialize `EventMetadata`. These are currently defined but not exported from the module's export list.

**Step 1: Add encodeJSON and decodeJSON to the export list**

Change:
```haskell
module Eventium.Store.Sql.JSONString
  ( JSONString,
    jsonStringCodec,
  )
where
```

To:
```haskell
module Eventium.Store.Sql.JSONString
  ( JSONString,
    jsonStringCodec,
    encodeJSON,
    decodeJSON,
  )
where
```

**Step 2: Build eventium-sql-common**

Run: `cabal build eventium-sql-common`
Expected: SUCCESS

**Step 3: Commit**

```bash
git add eventium-sql-common/src/Eventium/Store/Sql/JSONString.hs
git commit -m "feat(eventium-sql-common): export encodeJSON/decodeJSON from JSONString"
```

---

### Task 3: Update SqlEvent entity definition

**Files:**
- Modify: `eventium-sql-common/src/Eventium/Store/Sql/DefaultEntity.hs:24-75`

**Step 1: Remove unused imports, update entity definition**

Remove the `Data.Text (Text)` and `Data.Time (UTCTime)` imports (no longer needed since metadata fields are gone).

Change the Persistent entity from:
```haskell
share
  [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"]
  [persistLowerCase|
SqlEvent sql=events
    uuid UUID
    version EventVersion
    eventType Text
    event JSONString
    correlationId UUID Maybe
    causationId UUID Maybe
    createdAt UTCTime Maybe
    UniqueUuidVersion uuid version
    deriving Show
|]
```

To:
```haskell
share
  [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"]
  [persistLowerCase|
SqlEvent sql=events
    uuid UUID
    version EventVersion
    event JSONString
    metadata JSONString Maybe
    UniqueUuidVersion uuid version
    deriving Show
|]
```

**Step 2: Update defaultSqlEventStoreConfig**

The `SqlEvent` constructor will now be: `SqlEvent :: UUID -> EventVersion -> JSONString -> Maybe JSONString -> SqlEvent`

Change:
```haskell
defaultSqlEventStoreConfig :: SqlEventStoreConfig SqlEvent JSONString
defaultSqlEventStoreConfig =
  SqlEventStoreConfig
    { sqlEventStoreConfigSequenceMakeEntity = SqlEvent,
      sqlEventStoreConfigMakeKey = sqlEventMakeKey,
      sqlEventStoreConfigUnKey = sqlEventUnKey,
      sqlEventStoreConfigUUID = sqlEventUuid,
      sqlEventStoreConfigVersion = sqlEventVersion,
      sqlEventStoreConfigEventType = sqlEventEventType,
      sqlEventStoreConfigData = sqlEventPayload,
      sqlEventStoreConfigCorrelationId = sqlEventCorrelationId,
      sqlEventStoreConfigCausationId = sqlEventCausationId,
      sqlEventStoreConfigCreatedAt = sqlEventCreatedAt,
      sqlEventStoreConfigSequenceNumberField = SqlEventId,
      sqlEventStoreConfigUUIDField = SqlEventUuid,
      sqlEventStoreConfigVersionField = SqlEventVersion,
      sqlEventStoreConfigDataField = SqlEventPayload
    }
```

To:
```haskell
defaultSqlEventStoreConfig :: SqlEventStoreConfig SqlEvent JSONString
defaultSqlEventStoreConfig =
  SqlEventStoreConfig
    { sqlEventStoreConfigSequenceMakeEntity = SqlEvent,
      sqlEventStoreConfigMakeKey = sqlEventMakeKey,
      sqlEventStoreConfigUnKey = sqlEventUnKey,
      sqlEventStoreConfigUUID = sqlEventUuid,
      sqlEventStoreConfigVersion = sqlEventVersion,
      sqlEventStoreConfigData = sqlEventPayload,
      sqlEventStoreConfigMetadata = sqlEventMetadata,
      sqlEventStoreConfigSequenceNumberField = SqlEventId,
      sqlEventStoreConfigUUIDField = SqlEventUuid,
      sqlEventStoreConfigVersionField = SqlEventVersion,
      sqlEventStoreConfigDataField = SqlEventPayload
    }
```

Note: Do NOT build yet -- `SqlEventStoreConfig` itself still has the old shape. This file will compile after Task 4.

**Step 3: Commit (WIP, won't compile yet)**

```bash
git add eventium-sql-common/src/Eventium/Store/Sql/DefaultEntity.hs
git commit -m "wip(eventium-sql-common): update SqlEvent entity to use metadata jsonb column"
```

---

### Task 4: Update SqlEventStoreConfig and Operations

**Files:**
- Modify: `eventium-sql-common/src/Eventium/Store/Sql/Operations.hs:1-231`

**Step 1: Update imports**

Remove unused imports and add needed ones. The file currently imports:
```haskell
import Data.Text (Text)
import Data.Time (UTCTime)
```

Replace with:
```haskell
import Data.Maybe (fromMaybe, listToMaybe)
```

Remove the separate `Data.Text` and `Data.Time` imports (no longer needed in this module). Keep `Data.Maybe` but add `fromMaybe` to it (currently only imports `listToMaybe`).

Also add import for JSONString encoding/decoding:
```haskell
import Eventium.Store.Sql.JSONString (JSONString, encodeJSON, decodeJSON)
```

**Step 2: Simplify SqlEventStoreConfig**

Change:
```haskell
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
```

To:
```haskell
data SqlEventStoreConfig entity serialized
  = SqlEventStoreConfig
  { sqlEventStoreConfigSequenceMakeEntity :: UUID -> EventVersion -> serialized -> Maybe JSONString -> entity,
    -- Key manipulation
    sqlEventStoreConfigMakeKey :: SequenceNumber -> Key entity,
    sqlEventStoreConfigUnKey :: Key entity -> SequenceNumber,
    -- Record functions
    sqlEventStoreConfigUUID :: entity -> UUID,
    sqlEventStoreConfigVersion :: entity -> EventVersion,
    sqlEventStoreConfigData :: entity -> serialized,
    sqlEventStoreConfigMetadata :: entity -> Maybe JSONString,
    -- EntityFields
    sqlEventStoreConfigSequenceNumberField :: EntityField entity (Key entity),
    sqlEventStoreConfigUUIDField :: EntityField entity UUID,
    sqlEventStoreConfigVersionField :: EntityField entity EventVersion,
    sqlEventStoreConfigDataField :: EntityField entity serialized
  }
```

**Step 3: Update sqlEventToVersioned to decode metadata from JSONString**

Change:
```haskell
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
```

To:
```haskell
sqlEventToVersioned ::
  SqlEventStoreConfig entity serialized ->
  entity ->
  VersionedStreamEvent serialized
sqlEventToVersioned SqlEventStoreConfig {..} entity =
  StreamEvent
    (sqlEventStoreConfigUUID entity)
    (sqlEventStoreConfigVersion entity)
    (decodeMetadata $ sqlEventStoreConfigMetadata entity)
    (sqlEventStoreConfigData entity)
```

Add a helper function (local or module-level):
```haskell
decodeMetadata :: Maybe JSONString -> EventMetadata
decodeMetadata = fromMaybe (emptyMetadata "") . (>>= decodeJSON)
```

**Step 4: Update sqlGetProjectionIds**

The function currently uses `sqlEventStoreConfigSequenceMakeEntity` to get the table name, passing dummy arguments. Update the dummy arguments to match the new 4-arg constructor:

Change:
```haskell
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)
```

To:
```haskell
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined Nothing)
```

**Step 5: Update sqlMaxEventVersion**

Same pattern — update dummy arguments:

Change:
```haskell
  let tableName = FieldNameDB $ unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)
```

To:
```haskell
  let tableName = FieldNameDB $ unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined Nothing)
```

**Step 6: Update sqlStoreEvents (non-tagged writer)**

Change:
```haskell
sqlStoreEvents config@SqlEventStoreConfig {..} mLockCommand maxVersionSql uuid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities = zipWith (\v e -> sqlEventStoreConfigSequenceMakeEntity uuid v "" e Nothing Nothing Nothing) [versionNum + 1 ..] events
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length events)
  where
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 "" undefined Nothing Nothing Nothing)
```

To:
```haskell
sqlStoreEvents config@SqlEventStoreConfig {..} mLockCommand maxVersionSql uuid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities = zipWith (\v e -> sqlEventStoreConfigSequenceMakeEntity uuid v e Nothing) [versionNum + 1 ..] events
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  _ <- insertMany entities
  return $ versionNum + EventVersion (length events)
  where
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined Nothing)
```

**Step 7: Update sqlStoreEventsTagged**

Change:
```haskell
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
```

To:
```haskell
sqlStoreEventsTagged config@SqlEventStoreConfig {..} mLockCommand maxVersionSql uuid taggedEvents = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities =
        zipWith
          ( \v (TaggedEvent meta e) ->
              sqlEventStoreConfigSequenceMakeEntity
                uuid
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
    tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined Nothing)
```

**Step 8: Update unsafeSqlStoreGlobalStreamEvents**

Change:
```haskell
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
```

To:
```haskell
unsafeSqlStoreGlobalStreamEvents SqlEventStoreConfig {..} events =
  insertEntityMany $ fmap mkEventEntity events
  where
    mkEventEntity (StreamEvent () seqNum _ (StreamEvent uuid vers meta event)) =
      Entity
        (sqlEventStoreConfigMakeKey seqNum)
        ( sqlEventStoreConfigSequenceMakeEntity
            uuid
            vers
            event
            (Just $ encodeJSON meta)
        )
```

**Step 9: Update the module export list**

Remove `sqlStoreEventsTagged` ... wait, it's already exported. No changes needed to exports unless we want to export `decodeMetadata`. Keep it private.

**Step 10: Build eventium-sql-common**

Run: `cabal build eventium-sql-common`
Expected: SUCCESS

**Step 11: Commit**

```bash
git add eventium-sql-common/src/Eventium/Store/Sql/Operations.hs
git commit -m "feat(eventium-sql-common): update SqlEventStoreConfig and operations for metadata jsonb"
```

---

### Task 5: Update initializeSqliteEventStore

**Files:**
- Modify: `eventium-sqlite/src/Eventium/Store/Sqlite.hs:58-63`

**Step 1: Update dummy arguments in initializeSqliteEventStore**

Change:
```haskell
  let tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity undefined undefined undefined undefined undefined undefined undefined)
```

To:
```haskell
  let tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity undefined undefined undefined undefined)
```

**Step 2: Build eventium-sqlite**

Run: `cabal build eventium-sqlite`
Expected: SUCCESS

**Step 3: Commit**

```bash
git add eventium-sqlite/src/Eventium/Store/Sqlite.hs
git commit -m "fix(eventium-sqlite): update initializeSqliteEventStore for new entity shape"
```

---

### Task 6: Build all packages and run tests

**Step 1: Build all**

Run: `just build`
Expected: SUCCESS

**Step 2: Run all tests (except postgresql which needs a running DB)**

Run: `cabal test eventium-core && cabal test eventium-memory && cabal test eventium-sqlite && cabal test examples-bank`
Expected: PASS

**Step 3: Run hlint**

Run: `hlint eventium-core/src eventium-sql-common/src eventium-sqlite/src eventium-postgresql/src`
Expected: No new warnings

**Step 4: Run formatter**

Run: `just format`

**Step 5: Commit any formatting changes**

```bash
git add -A
git commit -m "style: format after metadata jsonb refactor"
```

---

### Task 7: Squash WIP commits

The Task 3 commit was marked WIP since it depended on Task 4 to compile. Now that everything compiles and tests pass, squash the WIP commit with the subsequent one.

**Step 1: Interactive rebase to squash**

Squash the WIP commit (`wip(eventium-sql-common): update SqlEvent entity...`) into the next commit (`feat(eventium-sql-common): update SqlEventStoreConfig...`) so the history is clean.

Alternatively, if the user prefers, leave as-is and squash on merge.
