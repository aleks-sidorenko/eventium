# Eventium PostgreSQL

PostgreSQL event store backend for Eventium.

## Overview

`eventium-postgresql` provides a production-grade event store backed by PostgreSQL.
It uses the `persistent` library for type-safe database access and guarantees
monotonically increasing global sequence numbers via `LOCK IN EXCLUSIVE MODE`.

## API

```haskell
postgresqlEventStoreWriter
  :: (MonadIO m)
  => SqlEventStoreConfig entity serialized
  -> VersionedEventStoreWriter (SqlPersistT m) serialized
```

Readers come from `eventium-sql-common` (re-exported):

```haskell
sqlEventStoreReader       :: SqlEventStoreConfig entity serialized -> VersionedEventStoreReader (SqlPersistT m) serialized
sqlGlobalEventStoreReader :: SqlEventStoreConfig entity serialized -> GlobalEventStoreReader (SqlPersistT m) serialized
```

## Usage

```haskell
import Eventium.Store.Postgresql
import Database.Persist.Postgresql

main :: IO ()
main = runStdoutLoggingT $
  withPostgresqlPool connStr 10 $ \pool -> do
    -- Use defaultSqlEventStoreConfig for the standard schema
    let writer = postgresqlEventStoreWriter defaultSqlEventStoreConfig
        reader = sqlEventStoreReader defaultSqlEventStoreConfig
    flip runSqlPool pool $ do
      runMigration migrateSqlEvent
      -- writer and reader are ready to use
```

## Setup

```bash
# Start PostgreSQL with docker-compose (from project root)
docker compose up -d

# Default connection settings (matching docker-compose.yaml):
# POSTGRES_HOST=127.0.0.1  POSTGRES_PORT=5432
# POSTGRES_USER=postgres   POSTGRES_PASSWORD=password
# POSTGRES_DBNAME=eventium_test
```

## Concurrency

PostgreSQL uses `LOCK table_name IN EXCLUSIVE MODE` during writes. This ensures
that auto-increment IDs are assigned in commit order, so concurrent readers
always see a gapless, monotonically increasing global sequence.

## Installation

```yaml
dependencies:
  - eventium-core
  - eventium-postgresql
  - persistent-postgresql
```

## Documentation

- [Main README](../README.md)
- [SQL Common](../eventium-sql-common/)
- [Design](../DESIGN.md)

## License

MIT -- see [LICENSE.md](LICENSE.md)
