# Eventium SQLite

SQLite event store backend for Eventium.

## Overview

`eventium-sqlite` provides a lightweight, file-based event store using SQLite.
Ideal for CLI tools, single-process applications, and development environments
where a full database server is unnecessary.

## API

```haskell
sqliteEventStoreWriter
  :: (MonadIO m)
  => SqlEventStoreConfig entity serialized
  -> VersionedEventStoreWriter (SqlPersistT m) serialized

initializeSqliteEventStore
  :: (MonadIO m)
  => SqlEventStoreConfig entity serialized
  -> ConnectionPool
  -> m ()
```

Readers come from `eventium-sql-common` (re-exported):

```haskell
sqlEventStoreReader       :: SqlEventStoreConfig entity serialized -> VersionedEventStoreReader (SqlPersistT m) serialized
sqlGlobalEventStoreReader :: SqlEventStoreConfig entity serialized -> GlobalEventStoreReader (SqlPersistT m) serialized
```

## Usage

```haskell
import Eventium.Store.Sqlite
import Database.Persist.Sqlite

main :: IO ()
main = runNoLoggingT $ do
  pool <- createSqlitePool "events.db" 1
  liftIO $ initializeSqliteEventStore defaultSqlEventStoreConfig pool

  let writer = sqliteEventStoreWriter defaultSqlEventStoreConfig
      reader = sqlEventStoreReader defaultSqlEventStoreConfig
  -- writer and reader operate in SqlPersistT m
```

`initializeSqliteEventStore` runs migrations and creates a UUID index.

## When to Use

| Scenario | Recommendation |
|----------|----------------|
| CLI tools, desktop apps | SQLite |
| Development / prototyping | SQLite or Memory |
| Multi-process / production | PostgreSQL |
| Unit tests (no persistence needed) | Memory |

## Installation

```yaml
dependencies:
  - eventium-core
  - eventium-sqlite
  - persistent-sqlite
```

## Documentation

- [Main README](../README.md)
- [SQL Common](../eventium-sql-common/)
- [Design](../DESIGN.md)
- [Cafe Example](../examples/cafe/) and [Bank Example](../examples/bank/) both use SQLite

## License

MIT -- see [LICENSE.md](LICENSE.md)
