# eventium-core Changelog

## 0.2.0 (Unreleased)

See the [root changelog](../CHANGELOG.md) for full details.

### Breaking changes

- `StreamEvent` now has 4 fields (added `EventMetadata`).
- `CommandHandler` gained an `err` type parameter; `commandHandlerHandler` renamed to `commandHandlerDecide`.
- `ProcessManager` is now pure: `processManagerReact` returns `[ProcessManagerEffect]`. `ProcessManagerEffect` only supports `IssueCommand` (removed `EmitEvent`).
- `EventPublisher`: removed `synchronousEventBusWrapper`; added `publishingEventStoreWriter` and `synchronousPublisher`.
- `PollingPeriodSeconds` (Double) replaced by `PollingIntervalMillis` (Int).
- Codec wrapper argument order changed (codec comes first).
- Removed `Eventium.ReadModel.Memory`.

### Additions

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
