# eventium-sqlite Changelog

## 0.2.0 (Unreleased)

- Updated for eventium-core 0.2.0 API changes.
- Single `events` table with metadata columns (event_type, correlation_id, causation_id, created_at).
- `initializeSqliteEventStore` creates UUID index on the events table.

## 0.1.0

Initial release.
