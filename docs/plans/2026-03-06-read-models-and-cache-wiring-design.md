# Read Models & ProjectionCache Wiring Design

## Problem

Two gaps in the current eventium library:

1. **No queryable read models** -- Global projections hold unbounded state in memory as opaque blobs. For use cases like bank transfers (queryable by status, date, etc.), we need persistent, indexed, user-defined views driven by events.

2. **No ProjectionCache wiring helpers** -- `VersionedProjectionCache` and `GlobalProjectionCache` exist but have no integration with command handling or event processing pipelines. Users must manually call `getLatestVersionedProjectionWithCache` and `updateVersionedProjectionCache`.

## Design

### 1. ProjectionCache Wiring Helpers (eventium-core)

Add helpers that integrate ProjectionCache into the command handling and event processing flow.

#### A. Snapshotting EventHandler

Auto-update the cache whenever events are written to a stream:

```haskell
snapshotEventHandler ::
  (Monad m) =>
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  VersionedStreamProjection state event ->
  EventHandler m (VersionedStreamEvent event)
```

Composable with `EventPublisher` so snapshots update on every write.

#### B. Cache-Aware CommandHandler Application

Like `applyCommandHandler` but loads from cache and updates cache after:

```haskell
applyCommandHandlerWithCache ::
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  CommandHandler state event command err ->
  UUID ->
  command ->
  m (Either (CommandHandlerError err) [VersionedStreamEvent event])
```

#### C. Global Variants

Equivalent helpers for `GlobalProjectionCache`:

```haskell
snapshotGlobalEventHandler ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  GlobalProjectionCache state m ->
  GlobalStreamProjection state event ->
  EventHandler m (GlobalStreamEvent event)
```

### 2. ReadModel Abstraction (eventium-core)

New record type bundling everything needed to maintain a queryable persistent view:

```haskell
data ReadModel m event = ReadModel
  { initialize :: m ()
  , eventHandler :: EventHandler m event
  , checkpointStore :: CheckpointStore m SequenceNumber
  , reset :: m ()
  }
```

**Fields:**
- `initialize` -- run migrations, create tables. Idempotent.
- `eventHandler` -- processes events and writes to user-defined tables.
- `checkpointStore` -- tracks last processed SequenceNumber.
- `reset` -- drop data + reset checkpoint (for full rebuilds).

**Combinators:**

```haskell
-- Subscribe and keep the view updated (blocking, runs forever)
runReadModel ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  ReadModel m event ->
  m ()

-- Reset + replay all events, then return (one-shot rebuild)
rebuildReadModel ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  ReadModel m event ->
  m ()

-- Combine multiple read models into one (fan-out events)
combineReadModels ::
  (Applicative m) =>
  [ReadModel m event] ->
  ReadModel m event
```

### 3. SQL CheckpointStore (eventium-sql-common)

New `CheckpointName` type (distinct from `ProjectionName`). Reuses the existing `projection_snapshots` table for storage:

```haskell
newtype CheckpointName = CheckpointName Text

sqlCheckpointStore ::
  (MonadIO m) =>
  CheckpointName ->
  CheckpointStore (SqlPersistT m) SequenceNumber
```

Backend re-exports:

```haskell
-- eventium-postgresql
postgresqlCheckpointStore ::
  (MonadIO m) => CheckpointName -> CheckpointStore (SqlPersistT m) SequenceNumber

-- eventium-sqlite
sqliteCheckpointStore ::
  (MonadIO m) => CheckpointName -> CheckpointStore (SqlPersistT m) SequenceNumber
```

### 4. Bank Example -- Transfer ReadModel

Demonstrates the full pattern with a queryable transfers view.

**User-defined table:**
- `transfers` -- `transfer_id UUID PK`, `source_account UUID`, `target_account UUID`, `amount Double`, `status Text`, `created_at UTCTime`, `updated_at UTCTime`

**Event handler logic:**
- `AccountTransferStarted` -> INSERT row with status "Pending"
- `AccountCreditedFromTransfer` -> UPDATE status to "Completed"
- `AccountTransferFailed` -> UPDATE status to "Failed"

**User-defined queries (not part of the library):**
- `getTransfersByStatus :: Status -> SqlPersistT m [Transfer]`
- `getTransfersByDateRange :: UTCTime -> UTCTime -> SqlPersistT m [Transfer]`

**Wiring:**
```haskell
transferReadModel :: ReadModel (SqlPersistT m) BankEvent
transferReadModel = ReadModel
  { initialize = runMigration migrateTransfers
  , eventHandler = handleTransferEvent
  , checkpointStore = postgresqlCheckpointStore (CheckpointName "transfers")
  , reset = deleteAllTransfers >> resetCheckpoint
  }
```

## What Stays Unchanged

- `ProjectionCache` (both Versioned and Global) -- kept as-is
- All existing implementations and tests
- `EventSubscription`, `EventPublisher` -- untouched

## Change Summary

| Change | Package | Description |
|--------|---------|-------------|
| Snapshotting EventHandler | eventium-core | Auto-update cache on events |
| Cache-aware CommandHandler | eventium-core | Load from cache + update after write |
| Global cache variants | eventium-core | Same helpers for GlobalProjectionCache |
| ReadModel type | eventium-core | New record with initialize, eventHandler, checkpointStore, reset |
| ReadModel combinators | eventium-core | runReadModel, rebuildReadModel, combineReadModels |
| CheckpointName type | eventium-sql-common | New newtype, distinct from ProjectionName |
| SQL CheckpointStore | eventium-sql-common | Reuses projection_snapshots table |
| Backend re-exports | eventium-postgresql, eventium-sqlite | postgresqlCheckpointStore, sqliteCheckpointStore |
| Transfer ReadModel | examples/bank | Queryable transfers with user-defined schema |
