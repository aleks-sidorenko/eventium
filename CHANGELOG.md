# eventium Changelog

## 0.2.0 (Unreleased)

Major refactoring of the core API.

### Breaking changes

- **StreamEvent** now carries `EventMetadata` (event type, correlation/causation IDs, timestamp):
  `StreamEvent key position metadata event` (was 3 fields, now 4).

- **CommandHandler** gained an explicit error type parameter:
  `CommandHandler state event command err`.
  - `commandHandlerHandler` renamed to `commandHandlerDecide`.
  - Returns `Either err [event]` instead of `[event]`.
  - `applyCommandHandler` returns `Either (CommandHandlerError err) [event]`
    where `CommandHandlerError` distinguishes `CommandRejected err` from
    `ConcurrencyConflict`.

- **ProcessManager** is now pure:
  - `processManagerReact :: state -> VersionedStreamEvent event -> [ProcessManagerEffect event command]`
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

### Additions

- `EventMetadata` type with `emptyMetadata` helper.
- `lenientCodecEventStoreReader` and `lenientCodecProjection` for
  graceful handling of unknown event types.
- `runProjectionSubscription` for maintaining projection state via polling.
- `eventHandlerMapMaybe` for filtering events before handling.

## 0.1.0

Initial Hackage release of `eventium`. Fork of `eventful`, Nix-ified and updated for GHC 9.6.7.
