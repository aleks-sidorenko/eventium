# Eventium SQL Common

Shared utilities and abstractions for SQL-based event stores.

## Overview

`eventium-sql-common` provides common code shared between `eventium-postgresql` and `eventium-sqlite`. It eliminates duplication and ensures consistency across SQL backend implementations.

## Purpose

This package is a **dependency** for SQL-based event stores. You typically don't use it directly—instead, depend on `eventium-postgresql` or `eventium-sqlite`, which automatically pull in `eventium-sql-common`.

## What's Included

### Persistent Entity Definitions
- **Event Table Schema** - Standard event table with stream keys, versions, and payloads
- **Global Event Table Schema** - Sequence-numbered global event ordering
- **Type-Safe Models** - Haskell types generated from Persistent schemas

### JSON Serialization
- **`JSONString`** - Wrapper for storing JSON as TEXT/VARCHAR
- **Aeson Integration** - Automatic JSON encoding/decoding
- **Event Payload Handling** - Type-safe event serialization

### Common Database Operations
- **Event Reading** - Query events by stream key, version, sequence
- **Event Writing** - Append events with concurrency control
- **Migration Support** - Shared schema migrations
- **Query Builders** - Reusable query patterns

### Utility Functions
- **UUID Handling** - Conversion between UUID types and database representations
- **Position Tracking** - EventVersion and SequenceNumber management
- **Error Handling** - Common SQL error types

## Architecture

```
eventium-postgresql ──┐
                      ├──> eventium-sql-common ──> eventium-core
eventium-sqlite ──────┘
```

Both PostgreSQL and SQLite implementations:
1. Use the same Persistent entity definitions
2. Share query logic where possible
3. Implement backend-specific optimizations where needed

## Files Structure

```
eventium-sql-common/
└── src/Eventium/Store/Sql/
    ├── JSONString.hs        # JSON wrapper type
    ├── Models.hs            # Persistent entity definitions
    ├── Queries.hs           # Common query functions
    └── Sql.hs               # Main module (re-exports)
```

## Usage

You don't typically depend on this package directly. Instead:

```yaml
# For PostgreSQL
dependencies:
  - eventium-core
  - eventium-postgresql  # Pulls in eventium-sql-common

# For SQLite
dependencies:
  - eventium-core
  - eventium-sqlite      # Pulls in eventium-sql-common
```

## For Library Authors

If you're building a new SQL-based backend:

```yaml
dependencies:
  - eventium-core
  - eventium-sql-common
  - persistent-yourdb
```

Then implement backend-specific optimizations while reusing common code:

```haskell
import Eventium.Store.Sql

-- Reuse common entity definitions
-- Customize queries for your database
-- Add database-specific optimizations
```

## Schema

### Event Table
```sql
CREATE TABLE event (
    id          SERIAL PRIMARY KEY,
    stream_key  TEXT NOT NULL,
    version     INTEGER NOT NULL,
    event_type  TEXT NOT NULL,
    payload     TEXT NOT NULL,  -- JSON
    created_at  TIMESTAMP NOT NULL,
    UNIQUE(stream_key, version)
);
```

### Global Event Table
```sql
CREATE TABLE global_event (
    sequence    SERIAL PRIMARY KEY,
    event_id    INTEGER NOT NULL REFERENCES event(id),
    created_at  TIMESTAMP NOT NULL
);
```

## Benefits

- ✅ **Code Reuse** - Write SQL logic once, use in multiple backends
- ✅ **Consistency** - Same schema and behavior across SQL stores
- ✅ **Type Safety** - Persistent generates type-safe database access
- ✅ **Maintainability** - Fix bugs in one place
- ✅ **Extensibility** - Easy to add new SQL backends

## Related Packages

- **`eventium-core`** - Core abstractions (storage-agnostic)
- **`eventium-postgresql`** - PostgreSQL implementation (uses this)
- **`eventium-sqlite`** - SQLite implementation (uses this)

## Documentation

- [Main README](../README.md) - Project overview
- [PostgreSQL Backend](../eventium-postgresql/) - Production SQL store
- [SQLite Backend](../eventium-sqlite/) - Embedded SQL store
- [Design Documentation](../DESIGN.md) - Architecture details

## License

MIT - see [LICENSE.md](LICENSE.md)

