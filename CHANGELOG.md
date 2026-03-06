# eventium Changelog

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
