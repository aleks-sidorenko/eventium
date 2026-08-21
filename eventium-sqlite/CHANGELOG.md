# eventium-sqlite Changelog

## 0.7.0

### Breaking changes

- `ProjectionCache` type parameters reordered to put the monad before the payload
  (`ProjectionCache key position m encoded`; `GlobalProjectionCache m encoded`,
  `VersionedProjectionCache m encoded`), following the `eventium-core` change —
  behaviour unchanged, update explicit type signatures.

## 0.6.1

- Raise `base` lower bound to `>= 4.20` (GHC 9.10) to match the supported toolchain.
- Add `homepage: https://eventium.dev`.

## 0.2.1 (Unreleased)

- `sqliteCheckpointStore` -- SQLite-backed `CheckpointStore` for `SequenceNumber` tracking.
- Re-exports `CheckpointName` from `eventium-sql-common`.

## 0.2.0 (Unreleased)

- Updated for eventium-core 0.2.0 API changes.
- Single `events` table with metadata columns (event_type, correlation_id, causation_id, created_at).
- `initializeSqliteEventStore` creates UUID index on the events table.
- Enabled `NoFieldSelectors`, `DuplicateRecordFields`, `OverloadedRecordDot` default extensions.

## 0.1.0

Initial release.
