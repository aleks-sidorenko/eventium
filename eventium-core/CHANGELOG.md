# eventium-core Changelog

## 0.6.2

### Additions

- `cachedProcessManagerEventHandler` -- like `processManagerEventHandler`, but
  reads and advances the process manager's global projection through a
  `GlobalProjectionCache` instead of replaying the entire global stream on every
  event. Each event folds only the events written since the last snapshot, so
  write-path latency no longer grows with the size of the event log. Correctness
  matches the uncached handler when the cache commits atomically with the write
  (e.g. a SQL-backed cache in the write transaction). Generic over event,
  command, state, and backend.

## 0.6.1

- Raise `base` lower bound to `>= 4.20` (GHC 9.10) to match the supported toolchain. Fixes a Hackage build failure where `foldl'` was not in scope on older `base` versions.
- Add `homepage: https://eventium.dev`.

## 0.2.1 (Unreleased)

### Additions

- `snapshotEventHandler` -- `EventHandler` that auto-updates a `VersionedProjectionCache` on each event.
- `snapshotGlobalEventHandler` -- same for `GlobalProjectionCache`.
- `applyCommandHandlerWithCache` -- like `applyCommandHandler` but loads from cache and updates after write.
- `Eventium.ReadModel` module:
  - `ReadModel` record type for queryable persistent views driven by the global event stream.
  - `runReadModel`, `rebuildReadModel`, `combineReadModels` combinators.

## 0.2.0 (Unreleased)

See the [root changelog](../CHANGELOG.md) for full details.

### Breaking changes

- Record field prefixes removed from all core types. Fields accessed via
  `OverloadedRecordDot` (e.g. `projection.seed`, `event.metadata`).
  `NoFieldSelectors` and `DuplicateRecordFields` enabled as default extensions.
- `StreamEvent` now has 4 fields (added `EventMetadata`).
- `CommandHandler` gained an `err` type parameter; `commandHandlerHandler` renamed to `decide`.
- `ProcessManager` is now pure: `react` returns `[ProcessManagerEffect]`. `ProcessManagerEffect` only supports `IssueCommand` (removed `EmitEvent`).
- `EventPublisher`: removed `synchronousEventBusWrapper`; added `publishingEventStoreWriter` and `synchronousPublisher`.
- `PollingPeriodSeconds` (Double) replaced by `PollingIntervalMillis` (Int).
- Codec wrapper argument order changed (codec comes first).
- Removed `Eventium.ReadModel.Memory`.

### Internal

- Absorbed `x-sum-type-boilerplate` into `Eventium.TH.SumType`.
- Dropped `persistent-template` dependency.

### Additions

- `Eventium.TH.SumType` module: `constructSumType`, `sumTypeConverter`,
  `partialSumTypeConverter` (previously from `x-sum-type-boilerplate`).
- `NoFieldSelectors`, `DuplicateRecordFields`, `OverloadedRecordDot` default extensions.
- `EventMetadata`, `emptyMetadata`.
- `lenientCodecEventStoreReader`, `lenientCodecProjection`.
- `runProjectionSubscription`, `eventHandlerMapMaybe`.
- `CommandDispatcher` newtype with `mkCommandDispatcher` and `fireAndForgetDispatcher`.
- `CommandDispatchResult` (`CommandSucceeded` | `CommandFailed Text`).
- `IssueCommandWithCompensation` effect for saga compensation workflows.
- `processManagerEventHandler` for wiring a `ProcessManager` to an `EventHandler`.
- `Eventium.CommandDispatcher` module: `AggregateHandler`, `mkAggregateHandler`,
  `commandHandlerDispatcher` for list-based multi-aggregate command routing.
- `embeddedCommandHandler` returns `Right []` for non-matching commands
  (was `DecodeError` exception).

## 0.1.0

Initial release.
