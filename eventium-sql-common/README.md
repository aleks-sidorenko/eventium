# Eventium SQL Common

Shared Persistent entity definitions and SQL operations for Eventium's SQL backends.

## Overview

`eventium-sql-common` provides the database schema, query logic, and serialization
helpers shared by `eventium-postgresql` and `eventium-sqlite`. You normally don't
depend on it directly -- it is pulled in by the backend packages.

## Schema

A single `events` table:

```sql
CREATE TABLE events (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,  -- global sequence number
    uuid            UUID    NOT NULL,
    version         INTEGER NOT NULL,
    event_type      TEXT    NOT NULL,
    event           JSONB   NOT NULL,
    correlation_id  UUID,
    causation_id    UUID,
    created_at      TIMESTAMPTZ,
    UNIQUE (uuid, version)
);
```

The auto-increment primary key serves as the global sequence number. Per-aggregate
ordering uses the `(uuid, version)` pair. No separate global events table exists.

## Key Exports

| Module | Purpose |
|--------|---------|
| `Eventium.Store.Sql.DefaultEntity` | Persistent TH-generated `SqlEvent` entity and `defaultSqlEventStoreConfig` |
| `Eventium.Store.Sql.Operations` | `SqlEventStoreConfig`, `sqlEventStoreReader`, `sqlGlobalEventStoreReader`, `sqlStoreEvents` |
| `Eventium.Store.Sql.JSONString` | `JSONString` newtype, `jsonStringCodec` |
| `Eventium.Store.Sql.Orphans` | `PersistField` instances for `UUID`, `EventVersion`, `SequenceNumber` |

`SqlEventStoreConfig` is a record that maps between Persistent entities and
Eventium types. `defaultSqlEventStoreConfig` works out of the box with the
default `SqlEvent` entity.

## For Backend Authors

If you're building a new SQL backend, depend on this package and provide a
backend-specific writer that calls `sqlStoreEvents` with appropriate locking:

```yaml
dependencies:
  - eventium-core
  - eventium-sql-common
  - persistent-yourdb
```

## Documentation

- [Main README](../README.md)
- [PostgreSQL Backend](../eventium-postgresql/)
- [SQLite Backend](../eventium-sqlite/)
- [Design](../DESIGN.md)

## License

MIT -- see [LICENSE.md](LICENSE.md)
