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

## 0.1.0

Initial release.
