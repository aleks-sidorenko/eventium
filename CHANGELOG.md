# eventium Changelog

## 0.7.0

### Breaking changes

- `ProjectionCache` type parameters reordered to put the monad before the payload
  (`ProjectionCache key position m encoded`; synonyms `VersionedProjectionCache m encoded`,
  `GlobalProjectionCache m encoded`), consistent with `EventStoreReader` /
  `EventStoreWriter`. Affects `eventium-core`, `eventium-sql-common`,
  `eventium-postgresql`, `eventium-sqlite`, `eventium-memory`, `eventium-testkit`.
  Behaviour is unchanged; update explicit type signatures.

### Added

- `cachedProcessManagerEventHandler` (`eventium-core`) -- snapshot-cached saga
  projection: folds only events written since the last snapshot instead of
  replaying the whole global stream on every event, so synchronous write-path
  saga projection no longer degrades with the event-log size.

## 0.6.0

### Added

- **Telemetry (`Eventium.Telemetry`)** — a generic, framework-free structured
  sink (`Telemetry m` over a `Signal` sum type) with a `silentTelemetry` no-op
  default. First subsystem wired: the write path, via
  `telemetryEventStoreWriter` (`Eventium.Store.Telemetry`), emitting
  `EventsPersisted` / `WriteConflict`.
- **`EventMetadata.custom :: Map Text Text`** — a generic per-event context bag
  (e.g. an app's user id), plus `insertCustomMetadata`. Injected via the existing
  `MetadataEnricher` seam.
- **`metadataEnrichingEventStoreWriterWithTag`** — caller-supplied per-event
  `EventTypeName` (for wrapper-sum events whose Typeable name isn't the useful
  discriminator).
- **`commandHandlerDispatcherWithTag`** — caller-supplied per-event
  `EventTypeName` for dispatcher/saga-emitted events.

### Changed

- `EventMetadata` JSON now **omits** absent/empty optional fields (the three
  `Maybe`s and `custom`) instead of emitting explicit `null`. Fully
  read-compatible: pre-existing rows with explicit `null`s still decode. New
  writes are leaner and no longer byte-identical to historical rows.

## 0.5.2

### Added

- **`eventTypeName` / `eventTypeNameOf`** (`Eventium.Store.Types`) — derive an
  `EventTypeName` from a type (`eventTypeName @MyEvent`) or a value
  (`eventTypeNameOf e`) via `Typeable`. Removes the `show . typeOf` boilerplate:
  clients keying a `SchemaRegistry` no longer write string literals, and the
  metadata-enriching writers (`Eventium.Store.Class`) now use it internally.

## 0.5.1

### Changed

- **Schema evolution module layout** now follows the library's @*.Types@ + instance
  convention. `Eventium.SchemaEvolution` is a re-export entry point;
  `Eventium.SchemaEvolution.Types` holds the representation-agnostic core
  (`SchemaRegistry`, `registerUpcasters`, `currentVersion`, `upcast`, `Upcaster`);
  `Eventium.SchemaEvolution.Json` holds the JSON instance (envelope, field
  combinators, `upcastingValueCodec`). Importing `Eventium.SchemaEvolution` still
  surfaces everything; import a submodule to limit scope.

## 0.5.0

### Additions

- **Event schema evolution (`Eventium.SchemaEvolution`)** — upcast-on-read against
  an immutable log, so a released app can change a stored event type's shape
  without breaking replay of older events. Stored bytes are never mutated.
  - Versioned envelope `{ "schemaVersion": N, "payload": <event-json> }`.
    Pre-envelope data (no `schemaVersion` key) is read as version 1.
  - `SchemaRegistry` of pure single-hop upcasters keyed by `EventTypeName`,
    ordered `[v1→v2, v2→v3, …]`; current version is `1 + length`. Version-skipping
    runs more hops.
  - `upcastingValueCodec eventTypeOf registry` — a drop-in `Codec a Value` that
    wraps the current-version envelope on encode and normalizes older events on
    decode. Serves every event type and backend, sync and async consumers, with a
    one-line call-site swap.
  - Upcaster combinators: `atKey`, `addFieldIfAbsent`, `renameField`,
    `removeField` — build field-level transforms without hand-rolled `KeyMap`
    manipulation.
- **`upcastingJsonStringCodec` (`eventium-sql-common`)** — the `Codec a JSONString`
  drop-in for `jsonStringCodec` at SQL reader/writer call sites.
- **`EventTypeName`** type alias (`Eventium.Store.Types`), shared by
  `EventMetadata.eventType` and the schema registry.

## 0.4.0

### Breaking

- **`EventStoreWriter` now reports the assigned global positions.** A successful
  write returns `EventWriteResult` (`= [(EventVersion, SequenceNumber)]`, one pair per
  event in write order) instead of just the end `EventVersion`. This exposes the
  real global `SequenceNumber`s the store assigns, so a synchronous subscriber can
  publish `GlobalStreamEvent`s with true positions and advance a `CheckpointStore`
  in the write transaction. `transactionalExpectedWriteHelper`'s store callback
  and all backend writers (postgresql, sqlite, memory) change accordingly.
  Accessors `versions`, `globalPositions`, `lastVersion`, `lastPosition` are
  provided on `EventWriteResult`.

### Additions

- **Global publishing** (`eventium-core`, `Eventium.EventPublisher`):
  - `GlobalEventPublisher` (with `Semigroup`/`Monoid`) — publishes
    `GlobalStreamEvent`s carrying real `SequenceNumber`s.
  - `publishingGlobalEventStoreWriter` / `publishingGlobalTaggedCodecEventStoreWriter`
    — wrap a writer to publish global events from the `EventWriteResult`.
  - `synchronousGlobalPublisher`, and `globalToVersionedHandler` to lift existing
    `VersionedStreamEvent` handlers (process managers, loggers) into a global
    publisher.
- **Dual-mode read models** (`eventium-core`, `Eventium.ReadModel`):
  - `readModelPublisher` — drive a `ReadModel` *synchronously* in the write
    transaction (apply handler + advance checkpoint), the counterpart to the async
    `runReadModel`. The same `ReadModel` value runs in either mode.
  - `catchUpReadModel` — one-shot catch-up from the current checkpoint *without*
    resetting (the startup/backfill counterpart; `rebuildReadModel` is now
    `reset` then `catchUpReadModel`).
  - `rebuildReadModel` now resets the checkpoint itself, so a `ReadModel`'s
    `reset` need only drop its view data (tables) — it no longer has to remember
    to zero the checkpoint. (Fixes a footgun where a `reset` that cleared data but
    not the checkpoint made `rebuildReadModel` replay from a stale position and
    project nothing.)
- **Testkit**: the shared store spec now asserts write-assigned global positions
  match the global reader's, on every backend.

## 0.2.1 (Unreleased)

### Additions

- **ProjectionCache wiring helpers** (`eventium-core`):
  - `snapshotEventHandler` -- `EventHandler` that auto-updates a `VersionedProjectionCache` on each event. Compose with `publishingEventStoreWriter` for transparent aggregate snapshotting.
  - `snapshotGlobalEventHandler` -- same for `GlobalProjectionCache`.
  - `applyCommandHandlerWithCache` -- like `applyCommandHandler` but loads from cache and updates after write.
- **ReadModel abstraction** (`eventium-core`):
  - `ReadModel` record type bundling initialization, event handling, checkpointing, and reset for queryable persistent views.
  - `runReadModel` -- polling subscription that keeps a read model updated (runs forever).
  - `rebuildReadModel` -- reset + replay all events (one-shot rebuild).
  - `combineReadModels` -- fan-out events to multiple read models.
  - ReadModels consume the global event stream exclusively (cross-aggregate views need total ordering).
- **SQL CheckpointStore** (`eventium-sql-common`):
  - `CheckpointName` newtype (distinct from `ProjectionName`) for semantic clarity.
  - `sqlCheckpointStore` -- SQL-backed `CheckpointStore` for `SequenceNumber`, reusing the `projection_snapshots` table.
- **Backend re-exports**:
  - `postgresqlCheckpointStore` (`eventium-postgresql`).
  - `sqliteCheckpointStore` (`eventium-sqlite`).
- **Transfer ReadModel example** (`examples/bank`):
  - `Bank.ReadModels.Transfers` -- persistent queryable view tracking transfer lifecycle (Pending/Completed/Failed) in a SQLite `transfers` table.
  - Query functions: `getTransfersByStatus`.
  - Demonstrates the full `ReadModel` pattern end-to-end.

## 0.2.0 (Unreleased)

Major refactoring of the core API.

### Breaking changes

- **Record field prefixes removed** across all packages. All record types now use
  short, unprefixed field names with `NoFieldSelectors`, `DuplicateRecordFields`,
  and `OverloadedRecordDot` extensions. Access fields via dot syntax
  (`projection.seed`, `event.metadata`). Key renames:
  - `Projection`: `projectionSeed` -> `seed`, `projectionEventHandler` -> `eventHandler`
  - `StreamProjection`: `streamProjectionKey` -> `key`, `streamProjectionState` -> `state`, etc.
  - `StreamEvent`: `streamEventKey` -> `key`, `streamEventPayload` -> `payload`, etc.
  - `EventMetadata`: `eventMetadataEventType` -> `eventType`, etc.
  - `CommandHandler`: `commandHandlerDecide` -> `decide`, `commandHandlerProjection` -> `projection`
  - `ProcessManager`: `processManagerProjection` -> `projection`, `processManagerReact` -> `react`
  - `QueryRange`: `queryRangeKey` -> `key`, `queryRangeStart` -> `start`, `queryRangeLimit` -> `limit`
  - `RetryConfig`: `retryInitialDelayMs` -> `initialDelayMs`, etc.
  - `DecodeError`/`EncodeError`: `decodeErrorContext` -> `context`, `decodeErrorMessage` -> `message`, etc.
  - `SqlEventStoreConfig`: all `sqlEventStoreConfig*` prefixes removed
  - Examples: lens-prefixed fields (`_accountBalance`, etc.) replaced with plain names

- **StreamEvent** now carries `EventMetadata` (event type, correlation/causation IDs, timestamp):
  `StreamEvent key position metadata event` (was 3 fields, now 4).

- **CommandHandler** gained an explicit error type parameter:
  `CommandHandler state event command err`.
  - `commandHandlerHandler` renamed to `decide`.
  - Returns `Either err [event]` instead of `[event]`.
  - `applyCommandHandler` returns `Either (CommandHandlerError err) [event]`
    where `CommandHandlerError` distinguishes `CommandRejected err` from
    `ConcurrencyConflict`.

- **ProcessManager** is now pure:
  - `react :: state -> VersionedStreamEvent event -> [ProcessManagerEffect event command]`
  - Effects are data: `IssueCommand UUID command`.
  - `runProcessManagerEffects` executes them.
  - Removed `ProcessManagerCommand`, pending-command/pending-event state fields.

- **EventPublisher** redesigned:
  - Removed `synchronousEventBusWrapper`.
  - Added `publishingEventStoreWriter` (wraps a writer to auto-publish) and
    `synchronousPublisher` (creates a publisher from an event handler).

- **EventSubscription** polling interval changed from `PollingPeriodSeconds` (`Double`) to
  `PollingIntervalMillis` (`Int`).

- **Codec wrappers** argument order changed -- codec comes first:
  - `codecProjection codec projection`
  - `codecCommandHandler eventCodec cmdCodec handler`

- **SQL schema**: unified single `events` table. The auto-increment primary key
  doubles as the global sequence number (no separate `global_event` table).
  Added `event_type`, `correlation_id`, `causation_id`, `created_at` columns.

- Removed `Eventium.ReadModel.Memory` module.

- **Examples**: removed `lens` dependency from bank example; replaced lens
  operations with `OverloadedRecordDot` and record update syntax.

### Internal

- **GHC upgraded from 9.6.7 to 9.10.3** (Stackage LTS 24.32).
- Absorbed `x-sum-type-boilerplate` into `Eventium.TH.SumType` (upstream
  incompatible with GHC 9.10).
- Dropped `persistent-template` dependency (merged into `persistent` since 2.12.0.1).

### Additions

- `NoFieldSelectors`, `DuplicateRecordFields`, and `OverloadedRecordDot` enabled
  as default extensions across all packages.
- `EventMetadata` type with `emptyMetadata` helper.
- `lenientCodecEventStoreReader` and `lenientCodecProjection` for
  graceful handling of unknown event types.
- `runProjectionSubscription` for maintaining projection state via polling.
- `eventHandlerMapMaybe` for filtering events before handling.
- **CommandDispatcher** newtype: wraps `UUID -> command -> m CommandDispatchResult`,
  replacing bare dispatch functions. `mkCommandDispatcher` and
  `fireAndForgetDispatcher` (for legacy callbacks) construct dispatchers.
- `CommandDispatchResult` (`CommandSucceeded` | `CommandFailed Text`): typed
  outcome from command dispatch, enabling compensation workflows.
- `IssueCommandWithCompensation` effect: extends `ProcessManagerEffect` with
  a compensation handler `(Text -> [ProcessManagerEffect command])` that fires
  on `CommandFailed`.
- `processManagerEventHandler`: wires a `ProcessManager` to a global reader
  and a `CommandDispatcher`, producing an `EventHandler` ready for use with
  `EventPublisher`.
- **CommandDispatcher module** (`Eventium.CommandDispatcher`): list-based command
  routing for multi-aggregate systems via `AggregateHandler` (existential wrapper)
  and `commandHandlerDispatcher`.
- `embeddedCommandHandler` now returns `Right []` for non-matching commands
  instead of throwing `DecodeError`, enabling safe multi-aggregate dispatching.

## 0.1.0

Initial Hackage release of `eventium`. Fork of `eventful`, Nix-ified and updated for GHC 9.6.
