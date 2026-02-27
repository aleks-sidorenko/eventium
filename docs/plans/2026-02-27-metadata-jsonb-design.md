# Metadata as JSONB

## Problem

The SQL event store table maps event metadata into individual columns (`event_type`, `correlation_id`, `causation_id`, `created_at`). This is rigid -- adding new metadata fields requires schema migrations. The `event_type` column is not populated by the non-tagged writer path (stores empty string).

## Decision

Replace all individual metadata columns with a single `metadata jsonb NULL` column, matching the approach already used for the event payload. `EventMetadata` remains a structured Haskell record, serialized to/from JSONB via Aeson.

## Schema

**Before:**
```sql
events(id, uuid, version, event_type, event, correlation_id, causation_id, created_at)
```

**After:**
```sql
events(id, uuid, version, event, metadata)
--                                ^^^^^^^^ jsonb NULL
```

`metadata` is `NULL` when written via the non-tagged writer path. When present (prefix stripped via `unPrefixLower`, consistent with event payload serialization):
```json
{
  "eventType": "BankAccountOpened",
  "correlationId": "550e8400-...",
  "causationId": null,
  "createdAt": "2026-02-27T12:00:00Z"
}
```

## Haskell type changes

### EventMetadata (eventium-core)

Stays structurally the same, gains `ToJSON`/`FromJSON`:

```haskell
data EventMetadata = EventMetadata
  { eventMetadataEventType :: !Text
  , eventMetadataCorrelationId :: !(Maybe UUID)
  , eventMetadataCausationId :: !(Maybe UUID)
  , eventMetadataCreatedAt :: !(Maybe UTCTime)
  } deriving (Show, Eq, Generic)

instance ToJSON EventMetadata
instance FromJSON EventMetadata
```

### SqlEvent entity (eventium-sql-common)

```
SqlEvent sql=events
    uuid UUID
    version EventVersion
    event JSONString
    metadata JSONString Maybe
    UniqueUuidVersion uuid version
```

### SqlEventStoreConfig (eventium-sql-common)

Loses 4 individual metadata accessors (`eventType`, `correlationId`, `causationId`, `createdAt`), gains 1:

```haskell
data SqlEventStoreConfig entity serialized = SqlEventStoreConfig
  { sqlEventStoreConfigSequenceMakeEntity :: UUID -> EventVersion -> serialized -> Maybe JSONString -> entity
  , sqlEventStoreConfigMakeKey :: SequenceNumber -> Key entity
  , sqlEventStoreConfigUnKey :: Key entity -> SequenceNumber
  , sqlEventStoreConfigUUID :: entity -> UUID
  , sqlEventStoreConfigVersion :: entity -> EventVersion
  , sqlEventStoreConfigData :: entity -> serialized
  , sqlEventStoreConfigMetadata :: entity -> Maybe JSONString
  , sqlEventStoreConfigSequenceNumberField :: EntityField entity (Key entity)
  , sqlEventStoreConfigUUIDField :: EntityField entity UUID
  , sqlEventStoreConfigVersionField :: EntityField entity EventVersion
  , sqlEventStoreConfigDataField :: EntityField entity serialized
  }
```

## Writer behavior

- **Non-tagged** (`sqlStoreEvents`): passes `Nothing` for metadata column
- **Tagged** (`sqlStoreEventsTagged`): encodes `EventMetadata` to `JSONString`, passes `Just jsonString`

## Reader behavior

- Decodes `Maybe JSONString` back to `EventMetadata`
- When `NULL`, defaults to `EventMetadata "" Nothing Nothing Nothing`

## Packages affected

1. **eventium-core** -- `ToJSON`/`FromJSON` on `EventMetadata`
2. **eventium-sql-common** -- entity definition, config record, read/write operations
3. **eventium-postgresql** -- types flow through, no logic changes
4. **eventium-sqlite** -- same
5. **examples/bank** -- uses `defaultSqlEventStoreConfig`, will adapt
6. **Tests** -- update references to old metadata fields

## Migration

Breaking schema change. No automatic data migration.
