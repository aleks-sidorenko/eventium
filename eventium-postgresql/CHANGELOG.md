# eventium-postgresql Changelog

## 0.2.0 (Unreleased)

- Updated for eventium-core 0.2.0 API changes.
- Single `events` table with metadata columns (event_type, correlation_id, causation_id, created_at).
- Uses `LOCK IN EXCLUSIVE MODE` for monotonic global sequence ordering.

## 0.1.0

Initial release.
