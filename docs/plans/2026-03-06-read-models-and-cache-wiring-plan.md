# Read Models & ProjectionCache Wiring Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add ProjectionCache wiring helpers (snapshotting event handler, cache-aware command handler) and a new ReadModel abstraction with SQL checkpoint support, demonstrated via a Transfer ReadModel in the bank example.

**Architecture:** Three layers of changes: (1) new helpers in eventium-core that wire ProjectionCache into event handling and command handling, (2) a new ReadModel record type with combinators in eventium-core, (3) SQL CheckpointStore in eventium-sql-common with backend re-exports, and (4) a Transfer ReadModel in the bank example demonstrating the full pattern.

**Tech Stack:** Haskell, Cabal, hspec, persistent, persistent-sqlite, persistent-postgresql

---

### Task 1: Add `snapshotEventHandler` to eventium-core

**Files:**
- Modify: `eventium-core/src/Eventium/ProjectionCache/Cache.hs`

**Step 1: Write the failing test**

Create test file `eventium-core/tests/Eventium/ProjectionCache/CacheSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Eventium.ProjectionCache.CacheSpec (spec) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Eventium
import Eventium.ProjectionCache.Memory
import Test.Hspec

type TestM = State TestState

data TestState = TestState
  { events :: [(UUID, [VersionedStreamEvent TestEvent])],
    cache :: ProjectionMap UUID EventVersion Int
  }

spec :: Spec
spec = do
  describe "snapshotEventHandler" $ do
    it "should update the cache when handling an event" $ do
      pending
```

**Note:** The actual test setup depends on the in-memory store. A simpler approach is to test this via the existing testkit infrastructure. Instead, add the function and test it in `eventium-memory` tests where the infrastructure already exists.

**Step 1 (revised): Add `snapshotEventHandler` function**

Add to `eventium-core/src/Eventium/ProjectionCache/Cache.hs`:

```haskell
-- | Creates an 'EventHandler' that updates a 'VersionedProjectionCache'
-- whenever events are received. This handler loads the latest projection
-- from the cache + store, then stores the updated snapshot.
--
-- Compose with 'synchronousPublisher' to auto-snapshot after writes:
--
-- @
-- let handler = snapshotEventHandler reader cache myStreamProjection
--     publisher = synchronousPublisher handler
--     writer = publishingEventStoreWriter rawWriter publisher
-- @
snapshotEventHandler ::
  (Monad m) =>
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  Projection state event ->
  EventHandler m (VersionedStreamEvent event)
snapshotEventHandler reader cache proj =
  EventHandler $ \streamEvent -> do
    let uuid = streamEvent.key
    updateVersionedProjectionCache reader cache (versionedStreamProjection uuid proj)
```

Also add the global variant:

```haskell
-- | Like 'snapshotEventHandler' but for 'GlobalProjectionCache'.
snapshotGlobalEventHandler ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  GlobalProjectionCache state m ->
  Projection state (VersionedStreamEvent event) ->
  EventHandler m (GlobalStreamEvent event)
snapshotGlobalEventHandler reader cache proj =
  EventHandler $ \_ ->
    updateGlobalProjectionCache reader cache (globalStreamProjection proj)
```

**Step 2: Update the export list**

Add `snapshotEventHandler` and `snapshotGlobalEventHandler` to the export list of `Eventium.ProjectionCache.Cache`.

**Step 3: Add import**

Add `import Eventium.EventHandler` to `Cache.hs`.

**Step 4: Verify it compiles**

Run: `cabal build eventium-core`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add eventium-core/src/Eventium/ProjectionCache/Cache.hs
git commit -m "feat(eventium-core): add snapshotEventHandler and snapshotGlobalEventHandler"
```

---

### Task 2: Add `applyCommandHandlerWithCache` to eventium-core

**Files:**
- Modify: `eventium-core/src/Eventium/CommandHandler.hs`

**Step 1: Add `applyCommandHandlerWithCache` function**

```haskell
-- | Like 'applyCommandHandler' but uses a 'VersionedProjectionCache' to
-- speed up aggregate loading. Loads the projection from cache (falling back
-- to replaying from the store), validates the command, writes events, and
-- updates the cache with the new state.
applyCommandHandlerWithCache ::
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  VersionedEventStoreReader m event ->
  VersionedProjectionCache state m ->
  CommandHandler state event command err ->
  UUID ->
  command ->
  m (Either (CommandHandlerError err) [event])
applyCommandHandlerWithCache writer reader cache cmdHandler uuid command = do
  sp <- getLatestVersionedProjectionWithCache reader cache (versionedStreamProjection uuid cmdHandler.projection)
  case cmdHandler.decide sp.state command of
    Left err -> return $ Left (CommandRejected err)
    Right events -> do
      result <- writer.storeEvents uuid (ExactPosition sp.position) events
      case result of
        Left writeErr -> return $ Left (ConcurrencyConflict writeErr)
        Right endVersion -> do
          let newPosition = endVersion
              newState = foldl' cmdHandler.projection.eventHandler sp.state events
          cache.storeSnapshot uuid newPosition newState
          return $ Right events
```

**Step 2: Add imports**

Add to imports in `CommandHandler.hs`:
```haskell
import Eventium.ProjectionCache.Cache
```

**Step 3: Update the export list**

Add `applyCommandHandlerWithCache` to the export list.

**Step 4: Verify it compiles**

Run: `cabal build eventium-core`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add eventium-core/src/Eventium/CommandHandler.hs
git commit -m "feat(eventium-core): add applyCommandHandlerWithCache"
```

---

### Task 3: Add tests for snapshotEventHandler and applyCommandHandlerWithCache

**Files:**
- Modify: `eventium-testkit/src/Eventium/Testkit.hs`
- Modify: `eventium-memory/tests/Eventium/ProjectionCache/MemorySpec.hs`

**Step 1: Add test runner and spec for snapshotEventHandler to testkit**

Add to `Eventium.Testkit`:

```haskell
snapshotEventHandlerSpec ::
  (Monad m) =>
  VersionedProjectionCacheRunner m ->
  Spec
snapshotEventHandlerSpec (VersionedProjectionCacheRunner withStoreAndCache) = do
  describe "snapshotEventHandler" $ do
    it "should update cache when handling events via publisher" $ do
      snapshot <- withStoreAndCache $ \writer reader cache -> do
        let handler = snapshotEventHandler reader cache counterProjection
            publisher = synchronousPublisher handler
            pubWriter = publishingEventStoreWriter writer publisher
        _ <- pubWriter.storeEvents uuid1 NoStream [Added 1, Added 2]
        cache.loadSnapshot uuid1
      snapshot `shouldBe` Just (1, Counter 3)

    it "should handle events for multiple aggregates" $ do
      (snap1, snap2) <- withStoreAndCache $ \writer reader cache -> do
        let handler = snapshotEventHandler reader cache counterProjection
            publisher = synchronousPublisher handler
            pubWriter = publishingEventStoreWriter writer publisher
        _ <- pubWriter.storeEvents uuid1 NoStream [Added 1, Added 2]
        _ <- pubWriter.storeEvents uuid2 NoStream [Added 10]
        s1 <- cache.loadSnapshot uuid1
        s2 <- cache.loadSnapshot uuid2
        return (s1, s2)
      snap1 `shouldBe` Just (1, Counter 3)
      snap2 `shouldBe` Just (0, Counter 10)
```

Export `snapshotEventHandlerSpec` from the testkit.

**Step 2: Add test runner and spec for applyCommandHandlerWithCache to testkit**

```haskell
applyCommandHandlerWithCacheSpec ::
  (Monad m) =>
  VersionedProjectionCacheRunner m ->
  Spec
applyCommandHandlerWithCacheSpec (VersionedProjectionCacheRunner withStoreAndCache) = do
  describe "applyCommandHandlerWithCache" $ do
    it "should apply a command and update the cache" $ do
      (result, snapshot) <- withStoreAndCache $ \writer reader cache -> do
        r <- applyCommandHandlerWithCache writer reader cache counterCommandHandler uuid1 (Increment 5)
        s <- cache.loadSnapshot uuid1
        return (r, s)
      result `shouldBe` Right [Added 5]
      snapshot `shouldBe` Just (0, Counter 5)

    it "should use cache for subsequent commands" $ do
      (result, snapshot) <- withStoreAndCache $ \writer reader cache -> do
        _ <- applyCommandHandlerWithCache writer reader cache counterCommandHandler uuid1 (Increment 5)
        r <- applyCommandHandlerWithCache writer reader cache counterCommandHandler uuid1 (Increment 3)
        s <- cache.loadSnapshot uuid1
        return (r, s)
      result `shouldBe` Right [Added 3]
      snapshot `shouldBe` Just (1, Counter 8)

    it "should reject commands and not update cache" $ do
      (result, snapshot) <- withStoreAndCache $ \writer reader cache -> do
        r <- applyCommandHandlerWithCache writer reader cache counterCommandHandler uuid1 (Increment 101)
        s <- cache.loadSnapshot uuid1
        return (r, s)
      result `shouldBe` Left (CommandRejected CounterOutOfBounds)
      snapshot `shouldBe` Nothing
```

Export `applyCommandHandlerWithCacheSpec` from the testkit.

**Step 3: Wire tests into memory backend**

In `eventium-memory/tests/Eventium/ProjectionCache/MemorySpec.hs`, add:

```haskell
  describe "snapshotEventHandler" $ snapshotEventHandlerSpec runner
  describe "applyCommandHandlerWithCache" $ applyCommandHandlerWithCacheSpec runner
```

Where `runner` is the existing `VersionedProjectionCacheRunner`.

**Step 4: Run tests to verify they fail**

Run: `cabal test eventium-memory`
Expected: FAIL (functions not yet wired, or tests should pass if code from Tasks 1-2 is already in place)

**Step 5: Run tests to verify they pass**

Run: `cabal test eventium-memory`
Expected: PASS

**Step 6: Commit**

```bash
git add eventium-testkit/src/Eventium/Testkit.hs eventium-memory/tests/Eventium/ProjectionCache/MemorySpec.hs
git commit -m "test: add snapshotEventHandler and applyCommandHandlerWithCache specs"
```

---

### Task 4: Add ReadModel type and combinators to eventium-core

**Files:**
- Create: `eventium-core/src/Eventium/ReadModel.hs`
- Modify: `eventium-core/src/Eventium.hs` (add re-export)

**Step 1: Create `Eventium.ReadModel` module**

```haskell
-- | Defines the ReadModel abstraction for queryable persistent views.
--
-- A 'ReadModel' bundles everything needed to maintain an event-driven
-- persistent view: initialization, event handling, checkpointing, and reset.
--
-- The library manages the event-to-handler pipeline and checkpointing.
-- Users define their own schema, write the 'EventHandler' that populates
-- tables, and query via their own code (SQL, persistent, etc.).
module Eventium.ReadModel
  ( ReadModel (..),
    runReadModel,
    rebuildReadModel,
    combineReadModels,
  )
where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import Eventium.EventHandler
import Eventium.EventSubscription (CheckpointStore (..), PollingIntervalMillis, delayMillis)
import Eventium.Store.Class

-- | A read model that maintains a queryable persistent view from events.
--
-- Users provide:
--
-- * 'initialize' — idempotent setup (run migrations, create tables)
-- * 'eventHandler' — processes global stream events, writes to user-defined storage
-- * 'checkpointStore' — tracks the last processed 'SequenceNumber'
-- * 'reset' — drop view data and reset checkpoint (for full rebuilds)
data ReadModel m event = ReadModel
  { initialize :: m (),
    eventHandler :: EventHandler m (GlobalStreamEvent event),
    checkpointStore :: CheckpointStore m SequenceNumber,
    reset :: m ()
  }

-- | Subscribe to the global event stream and keep the read model updated.
-- Runs forever, polling at the given interval.
runReadModel ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  PollingIntervalMillis ->
  ReadModel m event ->
  m ()
runReadModel globalReader pollIntervalMs rm = do
  rm.initialize
  forever $ pollReadModelOnce globalReader pollIntervalMs rm

-- | Reset the read model and replay all events from the beginning.
-- Returns after processing all currently available events.
rebuildReadModel ::
  (Monad m) =>
  GlobalEventStoreReader m event ->
  ReadModel m event ->
  m ()
rebuildReadModel globalReader rm = do
  rm.reset
  rm.initialize
  replayAll
  where
    replayAll = do
      latestSeq <- rm.checkpointStore.getCheckpoint
      newEvents <- globalReader.getEvents (eventsStartingAt () $ latestSeq + 1)
      case NE.nonEmpty newEvents of
        Nothing -> return ()
        Just ne -> do
          handleEvents rm.eventHandler newEvents
          rm.checkpointStore.saveCheckpoint (NE.last ne).position
          replayAll

-- | Combine multiple read models into one. Events are fanned out to all
-- handlers. Initialize and reset run all sub-models. The combined model
-- uses the checkpoint of the first model in the list (all models should
-- share the same checkpoint or be independently checkpointed).
--
-- For independently checkpointed read models, prefer running them
-- separately with 'runReadModel'.
combineReadModels ::
  (Applicative m) =>
  [ReadModel m event] ->
  ReadModel m event
combineReadModels rms =
  ReadModel
    { initialize = traverse_ (.initialize) rms,
      eventHandler = foldMap (.eventHandler) rms,
      checkpointStore = CheckpointStore
        { getCheckpoint = case rms of
            [] -> pure 0
            (rm : _) -> rm.checkpointStore.getCheckpoint,
          saveCheckpoint = \sn -> traverse_ (\rm -> rm.checkpointStore.saveCheckpoint sn) rms
        },
      reset = traverse_ (.reset) rms
    }
  where
    traverse_ f = foldr (\a b -> f a *> b) (pure ())

pollReadModelOnce ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  PollingIntervalMillis ->
  ReadModel m event ->
  m ()
pollReadModelOnce globalReader pollIntervalMs rm = do
  latestSeq <- rm.checkpointStore.getCheckpoint
  newEvents <- globalReader.getEvents (eventsStartingAt () $ latestSeq + 1)
  handleEvents rm.eventHandler newEvents
  case NE.nonEmpty newEvents of
    Nothing -> return ()
    Just ne -> rm.checkpointStore.saveCheckpoint (NE.last ne).position
  delayMillis pollIntervalMs
```

**Step 2: Export `delayMillis` from `EventSubscription`**

`delayMillis` is currently not exported from `Eventium.EventSubscription`. Either:
- Export it, or
- Duplicate the implementation in `ReadModel` (just `liftIO $ threadDelay (ms * 1000)`)

Preferred: duplicate the one-liner to avoid coupling ReadModel to EventSubscription internals.

Replace the import and add locally:
```haskell
import Control.Concurrent (threadDelay)

delayMillis :: (MonadIO m) => Int -> m ()
delayMillis ms = liftIO $ threadDelay (ms * 1000)
```

**Step 3: Add re-export from `Eventium.hs`**

Add to `eventium-core/src/Eventium.hs`:
```haskell
import Eventium.ReadModel as X
```

**Step 4: Verify it compiles**

Run: `cabal build eventium-core`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add eventium-core/src/Eventium/ReadModel.hs eventium-core/src/Eventium.hs
git commit -m "feat(eventium-core): add ReadModel type and combinators"
```

---

### Task 5: Add `CheckpointName` and `sqlCheckpointStore` to eventium-sql-common

**Files:**
- Modify: `eventium-sql-common/src/Eventium/ProjectionCache/Sql.hs`

**Step 1: Add `CheckpointName` newtype and `sqlCheckpointStore`**

Add to `Eventium.ProjectionCache.Sql`:

```haskell
-- | A name identifying a checkpoint in the projection snapshots table.
-- Distinct from 'ProjectionName' to maintain semantic clarity — checkpoint
-- stores track subscription position, while projection caches store
-- aggregate snapshots.
newtype CheckpointName = CheckpointName Text
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql)

-- | A SQL-backed 'CheckpointStore' for tracking 'SequenceNumber' position.
--
-- Reuses the @projection_snapshots@ table. Stores one row with the
-- 'CheckpointName' as the projection name and 'nil' UUID as the entity key.
-- The @position@ column stores the sequence number; the @state@ column
-- stores an empty JSON string.
sqlCheckpointStore ::
  (MonadIO m) =>
  CheckpointName ->
  CheckpointStore (SqlPersistT m) SequenceNumber
sqlCheckpointStore (CheckpointName name) =
  let projName = ProjectionName name
   in CheckpointStore
        { getCheckpoint = do
            mEntity <- get (ProjectionSnapshotEntityKey projName nil)
            return $ maybe 0 (SequenceNumber . (.position)) mEntity,
          saveCheckpoint = \(SequenceNumber sn) -> do
            now <- liftIO getCurrentTime
            repsert (ProjectionSnapshotEntityKey projName nil) $
              ProjectionSnapshotEntity
                { projectionName = projName,
                  entityId = nil,
                  position = sn,
                  state = "{}", -- empty JSON, not used for checkpoints
                  updatedAt = now
                }
        }
```

**Step 2: Add required imports**

Add to imports:
```haskell
import Eventium.EventSubscription (CheckpointStore (..))
import Eventium.Store.Class (SequenceNumber (..))
```

Note: `SequenceNumber` is likely already imported. `CheckpointStore` needs to be added.

**Step 3: Update export list**

Add `CheckpointName(..)` and `sqlCheckpointStore` to the export list.

**Step 4: Verify it compiles**

Run: `cabal build eventium-sql-common`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add eventium-sql-common/src/Eventium/ProjectionCache/Sql.hs
git commit -m "feat(eventium-sql-common): add CheckpointName and sqlCheckpointStore"
```

---

### Task 6: Add backend re-exports for CheckpointStore

**Files:**
- Modify: `eventium-postgresql/src/Eventium/ProjectionCache/Postgresql.hs`
- Modify: `eventium-sqlite/src/Eventium/ProjectionCache/Sqlite.hs`

**Step 1: Add PostgreSQL re-export**

Add to `Eventium.ProjectionCache.Postgresql`:

```haskell
import Eventium.ProjectionCache.Sql (CheckpointName, sqlCheckpointStore)
import Eventium.EventSubscription (CheckpointStore)

-- | PostgreSQL-backed 'CheckpointStore' for tracking subscription position.
-- Alias for 'sqlCheckpointStore'.
postgresqlCheckpointStore ::
  (MonadIO m) =>
  CheckpointName ->
  CheckpointStore (SqlPersistT m) SequenceNumber
postgresqlCheckpointStore = sqlCheckpointStore
```

Add `postgresqlCheckpointStore`, `CheckpointName` to the export list.

**Step 2: Add SQLite re-export**

Add to `Eventium.ProjectionCache.Sqlite`:

```haskell
import Eventium.ProjectionCache.Sql (CheckpointName, sqlCheckpointStore)
import Eventium.EventSubscription (CheckpointStore)

-- | SQLite-backed 'CheckpointStore' for tracking subscription position.
-- Alias for 'sqlCheckpointStore'.
sqliteCheckpointStore ::
  (MonadIO m) =>
  CheckpointName ->
  CheckpointStore (SqlPersistT m) SequenceNumber
sqliteCheckpointStore = sqlCheckpointStore
```

Add `sqliteCheckpointStore`, `CheckpointName` to the export list.

**Step 3: Verify both compile**

Run: `cabal build eventium-postgresql && cabal build eventium-sqlite`
Expected: SUCCESS

**Step 4: Commit**

```bash
git add eventium-postgresql/src/Eventium/ProjectionCache/Postgresql.hs eventium-sqlite/src/Eventium/ProjectionCache/Sqlite.hs
git commit -m "feat(eventium-postgresql, eventium-sqlite): add CheckpointStore re-exports"
```

---

### Task 7: Add CheckpointStore tests

**Files:**
- Modify: `eventium-testkit/src/Eventium/Testkit.hs`
- Modify: `eventium-sqlite/tests/Eventium/ProjectionCache/SqliteSpec.hs`
- Modify: `eventium-postgresql/tests/Eventium/ProjectionCache/PostgresqlSpec.hs`

**Step 1: Add CheckpointStore test runner and spec to testkit**

```haskell
newtype CheckpointStoreRunner m
  = CheckpointStoreRunner
      ( forall a.
        ( CheckpointStore m SequenceNumber ->
          m a
        ) ->
        IO a
      )

checkpointStoreSpec ::
  (Monad m) =>
  CheckpointStoreRunner m ->
  Spec
checkpointStoreSpec (CheckpointStoreRunner withCheckpoint) = do
  describe "CheckpointStore" $ do
    it "should return 0 when no checkpoint exists" $ do
      pos <- withCheckpoint $ \cs -> cs.getCheckpoint
      pos `shouldBe` 0

    it "should store and retrieve a checkpoint" $ do
      pos <- withCheckpoint $ \cs -> do
        cs.saveCheckpoint 42
        cs.getCheckpoint
      pos `shouldBe` 42

    it "should update an existing checkpoint" $ do
      pos <- withCheckpoint $ \cs -> do
        cs.saveCheckpoint 10
        cs.saveCheckpoint 20
        cs.getCheckpoint
      pos `shouldBe` 20
```

Export `CheckpointStoreRunner(..)` and `checkpointStoreSpec` from the testkit.

**Step 2: Wire into SQLite tests**

In `eventium-sqlite/tests/Eventium/ProjectionCache/SqliteSpec.hs`, add a `CheckpointStoreRunner` using `sqliteCheckpointStore` and call `checkpointStoreSpec`.

**Step 3: Wire into PostgreSQL tests**

Same pattern in `eventium-postgresql/tests/Eventium/ProjectionCache/PostgresqlSpec.hs`.

**Step 4: Run tests**

Run: `cabal test eventium-sqlite && cabal test eventium-postgresql`
Expected: PASS

**Step 5: Commit**

```bash
git add eventium-testkit/src/Eventium/Testkit.hs eventium-sqlite/tests/ eventium-postgresql/tests/
git commit -m "test: add CheckpointStore specs for SQLite and PostgreSQL"
```

---

### Task 8: Add Transfer ReadModel to bank example

**Files:**
- Create: `examples/bank/src/Bank/ReadModels/Transfers.hs`
- Modify: `examples/bank/src/Bank/Models.hs` (re-export if needed)
- Modify: `examples/bank/package.yaml` (add persistent dependency if not present)

**Step 1: Create Transfer ReadModel module**

Create `examples/bank/src/Bank/ReadModels/Transfers.hs`. This module defines:

1. A Persistent entity for the transfers table
2. An `EventHandler` that processes `BankEvent` and writes transfer rows
3. Query functions (`getTransfersByStatus`, `getTransfersByDateRange`)
4. A `ReadModel` wiring function

The exact implementation depends on the bank example's existing persistent/SQLite setup. Review `Bank/CLI/Store.hs` and `Bank/Models.hs` to match conventions.

Key structure:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bank.ReadModels.Transfers
  ( TransferEntity (..),
    migrateTransfer,
    transferReadModel,
    getTransfersByStatus,
    getTransfersByDateRange,
  )
where

import Bank.Models
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Eventium
import Eventium.ProjectionCache.Sqlite (CheckpointName (..), sqliteCheckpointStore)

share
  [mkPersist sqlSettings {mpsFieldLabelModifier = const id}, mkMigrate "migrateTransfer"]
  [persistLowerCase|
TransferEntity sql=transfers
    transferId UUID
    sourceAccount UUID
    targetAccount UUID Maybe
    amount Double
    status Text
    createdAt UTCTime Maybe
    updatedAt UTCTime Maybe
    UniqueTransferId transferId
    deriving Show
|]

transferReadModel :: ReadModel (SqlPersistT IO) BankEvent
transferReadModel =
  ReadModel
    { initialize = runMigration migrateTransfer,
      eventHandler = handleTransferEvent,
      checkpointStore = sqliteCheckpointStore (CheckpointName "transfers"),
      reset = deleteWhere ([] :: [Filter TransferEntity]) -- delete all transfers
    }

handleTransferEvent :: EventHandler (SqlPersistT IO) (GlobalStreamEvent BankEvent)
handleTransferEvent = EventHandler $ \globalEvent -> do
  let innerEvent = globalEvent.payload
  case innerEvent.payload of
    AccountTransferStartedBankEvent evt ->
      insert_ $
        TransferEntity
          { transferId = evt.transferId,
            sourceAccount = innerEvent.key,
            targetAccount = Just evt.targetAccount,
            amount = evt.amount,
            status = "Pending",
            createdAt = innerEvent.metadata.createdAt,
            updatedAt = innerEvent.metadata.createdAt
          }
    AccountTransferCompletedBankEvent evt ->
      updateWhere
        [TransferEntityTransferId ==. evt.transferId]
        [TransferEntityStatus =. "Completed", TransferEntityUpdatedAt =. innerEvent.metadata.createdAt]
    AccountTransferFailedBankEvent evt ->
      updateWhere
        [TransferEntityTransferId ==. evt.transferId]
        [TransferEntityStatus =. ("Failed: " <> pack evt.reason), TransferEntityUpdatedAt =. innerEvent.metadata.createdAt]
    _ -> return ()

-- | Query transfers by status.
getTransfersByStatus :: (MonadIO m) => Text -> SqlPersistT m [Entity TransferEntity]
getTransfersByStatus s = selectList [TransferEntityStatus ==. s] []

-- | Query transfers by date range.
getTransfersByDateRange :: (MonadIO m) => UTCTime -> UTCTime -> SqlPersistT m [Entity TransferEntity]
getTransfersByDateRange from to =
  selectList [TransferEntityCreatedAt >=. Just from, TransferEntityCreatedAt <=. Just to] []
```

**Note:** The exact event constructor names (e.g., `AccountTransferStartedBankEvent`) depend on the TH-generated sum type. Check `Bank.Models` for the actual constructor names. The `BankEvent` sum type and its constructors need to be verified.

**Step 2: Check the BankEvent sum type constructors**

Read `Bank/Models.hs` and the TH-generated constructors to get the right names.

**Step 3: Update package.yaml if needed**

Verify `examples/bank/package.yaml` has `persistent` and related deps. It likely does since it uses SQLite already.

**Step 4: Verify it compiles**

Run: `cabal build examples-bank`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add examples/bank/src/Bank/ReadModels/Transfers.hs
git commit -m "feat(examples-bank): add Transfer ReadModel with queryable transfers"
```

---

### Task 9: Add Transfer ReadModel test

**Files:**
- Create or modify: `examples/bank/tests/Bank/ReadModels/TransfersSpec.hs`

**Step 1: Write the test**

```haskell
module Bank.ReadModels.TransfersSpec (spec) where

import Bank.Models
import Bank.ReadModels.Transfers
import Database.Persist.Sqlite
import Eventium
import Test.Hspec

spec :: Spec
spec = do
  describe "Transfer ReadModel" $ do
    it "should track transfer lifecycle" $ do
      -- Set up in-memory SQLite with event store + transfer read model
      -- Insert transfer events
      -- Query by status and verify
      pending
```

The exact test setup depends on how the bank example sets up its test infrastructure. This test should:
1. Create an in-memory SQLite pool
2. Initialize the event store and transfer read model
3. Write transfer events
4. Call `rebuildReadModel` or process events via the handler
5. Query and assert

**Step 2: Run tests**

Run: `cabal test examples-bank`
Expected: PASS

**Step 3: Commit**

```bash
git add examples/bank/tests/Bank/ReadModels/TransfersSpec.hs
git commit -m "test(examples-bank): add Transfer ReadModel test"
```

---

### Task 10: Run full test suite and format

**Step 1: Format all code**

Run: `just format`

**Step 2: Run hpack**

Run: `just hpack`

**Step 3: Run full test suite**

Run: `just test`
Expected: ALL PASS

**Step 4: Fix any compilation errors or test failures**

Address issues as they arise.

**Step 5: Final commit**

```bash
git add -A
git commit -m "chore: format and regenerate cabal files"
```
