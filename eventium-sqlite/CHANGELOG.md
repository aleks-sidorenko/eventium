# eventium-sqlite Changelog

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
