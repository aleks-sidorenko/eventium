# Eventium PostgreSQL

PostgreSQL-based event store implementation for production event sourcing systems.

## Overview

`eventium-postgresql` provides a robust, production-ready event store implementation backed by PostgreSQL. It leverages PostgreSQL's ACID guarantees, indexing capabilities, and reliability for persistent event storage with high performance and data integrity.

## Features

- ✅ **ACID Transactions** - Full consistency guarantees
- ✅ **Optimistic Concurrency** - Prevents lost updates with version checks
- ✅ **Efficient Indexing** - Fast event retrieval by stream and position
- ✅ **Global Event Ordering** - Sequence numbers for time-ordered queries
- ✅ **Type-Safe Access** - Uses Persistent library for database operations
- ✅ **Production Ready** - Battle-tested PostgreSQL backend
- ✅ **Multi-Process Support** - Concurrent access from multiple applications

## Database Schema

The implementation creates two main tables:
- **Events Table** - Stores events with aggregate keys, versions, and payloads
- **Global Events Table** - Maintains global ordering with sequence numbers

Indexes ensure fast lookups by:
- Stream key + version
- Global sequence number
- Event types (for projections)

## Installation

Add to your `package.yaml`:

```yaml
dependencies:
  - eventium-core
  - eventium-sql-common
  - eventium-postgresql
  - persistent-postgresql  # PostgreSQL driver
```

## Usage

```haskell
import Eventium.Store.Postgresql
import Database.Persist.Postgresql

main :: IO ()
main = do
  let connStr = "host=localhost dbname=eventstore user=postgres"
  
  withPostgresqlPool connStr 10 $ \pool -> do
    -- Initialize schema
    flip runSqlPool pool $ do
      runMigration migrateAll
      
      -- Create event store
      let store = makePostgresqlEventStore pool
      
      -- Use with command handlers
      result <- applyCommandHandler 
        (eventStoreWriter store)
        (eventStoreReader store)
        commandHandler
        aggregateId
        command
```

## Configuration

### Connection String

```haskell
-- Basic connection
"host=localhost port=5432 dbname=mydb user=myuser password=mypass"

-- With connection pool
withPostgresqlPool connectionString poolSize $ \pool -> ...
```

### Connection Pooling

Recommended settings for production:
```haskell
-- Pool size based on concurrent requests
poolSize = numCores * 2 + effectiveSpindleCount

-- Example: 10 connections for typical web app
withPostgresqlPool connStr 10 $ \pool -> ...
```

## Setup

### Start PostgreSQL with Docker

```bash
# Using docker-compose (provided in project root)
docker-compose up -d postgres

# Or manually
docker run -d \
  --name eventium-postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=eventstore \
  -p 5432:5432 \
  postgres:15
```

### Run Migrations

```haskell
runSqlPool (runMigration migrateAll) pool
```

## Performance

PostgreSQL provides excellent performance characteristics:
- **Writes**: ~1000-5000 events/sec (single connection)
- **Reads**: ~10000-50000 events/sec (with proper indexing)
- **Scalability**: Read replicas for query scaling

See `postgres-event-store-bench/` for benchmarking scripts.

## Best Practices

1. **Use Connection Pooling** - Essential for web applications
2. **Index Strategy** - Default indexes cover common queries
3. **Backup Strategy** - Regular PostgreSQL backups
4. **Monitoring** - Watch connection pool usage and query performance
5. **Read Replicas** - Scale read models with PostgreSQL replication

## Production Considerations

- **High Availability** - Use PostgreSQL replication
- **Backup & Recovery** - Point-in-time recovery with WAL archiving
- **Monitoring** - Track event growth and query performance
- **Connection Limits** - Configure max_connections appropriately

## Example: Complete Setup

```haskell
import Eventium.Store.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)

setupEventStore :: IO ()
setupEventStore = runStdoutLoggingT $ do
  let connStr = "host=localhost dbname=eventstore"
  
  withPostgresqlPool connStr 10 $ \pool -> do
    -- Run migrations
    flip runSqlPool pool $ runMigration migrateAll
    
    -- Event store is ready to use
    liftIO $ putStrLn "Event store initialized"
```

## Documentation

- [Main README](../README.md) - Project overview
- [SQL Common](../eventium-sql-common/) - Shared SQL utilities
- [Design Documentation](../DESIGN.md) - Architecture details

## License

MIT - see [LICENSE.md](LICENSE.md)
