# eventium-memory Changelog

## 0.6.1

- Raise `base` lower bound to `>= 4.20` (GHC 9.10) to match the supported toolchain.
- Add `homepage: https://eventium.dev`.

## 0.2.1 (Unreleased)

- Tests for `snapshotEventHandler`, `snapshotGlobalEventHandler`, `applyCommandHandlerWithCache`.
- Tests for `rebuildReadModel` and `combineReadModels`.

## 0.2.0 (Unreleased)

- Updated for eventium-core 0.2.0 API changes (`StreamEvent` with metadata, new `EventVersion`/`SequenceNumber` types).
- `EventMap` stores `VersionedStreamEvent event` values with full metadata.
- Enabled `NoFieldSelectors`, `DuplicateRecordFields`, `OverloadedRecordDot` default extensions.
- Removed record field prefixes from internal types.

## 0.1.0

Initial release.
