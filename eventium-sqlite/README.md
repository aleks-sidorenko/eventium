# Eventium SQLite

SQLite-based event store implementation for embedded and single-process applications.

## Overview

`eventium-sqlite` provides a lightweight, file-based event store implementation using SQLite. It's perfect for single-process applications, embedded systems, mobile apps, and scenarios where you want persistent storage without the complexity of a database server.

## Features

- ✅ **Zero Configuration** - No database server required
- ✅ **File-Based Storage** - Single database file for easy backup
- ✅ **ACID Transactions** - Full consistency guarantees from SQLite
- ✅ **Optimistic Concurrency** - Version-based conflict detection
- ✅ **Type-Safe Access** - Uses Persistent library
- ✅ **Cross-Platform** - Works on Linux, macOS, Windows
- ✅ **Embedded-Friendly** - Low resource footprint
- ✅ **Easy Deployment** - No separate database process

## When to Use SQLite

### ✅ Good Fit
- **Desktop Applications** - Local data storage
- **CLI Tools** - Persistent command-line applications
- **Mobile Apps** - Embedded event storage
- **Development/Testing** - Persistent data without server setup
- **Single-Process Systems** - No concurrent process access needed
- **Edge Computing** - Resource-constrained environments

### ⚠️ Consider Alternatives
- **Multi-Process Systems** - Use `eventium-postgresql` instead
- **High Write Concurrency** - PostgreSQL handles concurrent writes better
- **Distributed Systems** - Need a client-server database
- **Very Large Datasets** - PostgreSQL scales better for TB+ data

## Installation

Add to your `package.yaml`:

```yaml
dependencies:
  - eventium-core
  - eventium-sql-common
  - eventium-sqlite
  - persistent-sqlite  # SQLite driver
```

## Usage

```haskell
import Eventium.Store.Sqlite
import Database.Persist.Sqlite

main :: IO ()
main = do
  -- Use file-based storage
  withSqlitePool "events.db" 1 $ \pool -> do
    -- Initialize schema
    flip runSqlPool pool $ do
      runMigration migrateAll
      
      -- Create event store
      let store = makeSqliteEventStore pool
      
      -- Use with command handlers
      result <- applyCommandHandler 
        (eventStoreWriter store)
        (eventStoreReader store)
        commandHandler
        aggregateId
        command
```

## Database Location

### File-Based Storage
```haskell
-- Relative path
withSqlitePool "events.db" 1 $ \pool -> ...

-- Absolute path
withSqlitePool "/var/lib/myapp/events.db" 1 $ \pool -> ...

-- User-specific location
home <- getHomeDirectory
let dbPath = home </> ".myapp" </> "events.db"
withSqlitePool dbPath 1 $ \pool -> ...
```

### In-Memory Storage (Testing)
```haskell
-- Temporary in-memory database
withSqlitePool ":memory:" 1 $ \pool -> ...
```

## Configuration

### Connection Pool
SQLite works best with a single connection per process:

```haskell
-- Recommended for SQLite
withSqlitePool "events.db" 1 $ \pool -> ...
```

### WAL Mode (Recommended)
Enable Write-Ahead Logging for better concurrent read performance:

```haskell
withSqlitePool "events.db" 1 $ \pool -> do
  flip runSqlPool pool $ do
    rawExecute "PRAGMA journal_mode=WAL;" []
    runMigration migrateAll
```

Benefits:
- Readers don't block writers
- Better performance for read-heavy workloads
- Safer concurrent access

## Performance

Typical SQLite event store performance:
- **Writes**: ~1000-3000 events/sec
- **Reads**: ~5000-20000 events/sec
- **Storage**: ~1KB per event (JSON serialized)

### Optimization Tips

1. **Use WAL Mode** - Better concurrent access
2. **Batch Writes** - Multiple events per transaction
3. **Index Strategy** - Default indexes cover common queries
4. **VACUUM Regularly** - Reclaim space from deleted data
5. **Synchronous Mode** - Balance durability vs speed

## Backup & Recovery

### Simple File Copy
```bash
# Stop application or ensure no writes
cp events.db events.db.backup

# Or use SQLite backup command
sqlite3 events.db ".backup events.db.backup"
```

### Continuous Backup
```bash
# With WAL mode, backup while app runs
sqlite3 events.db ".backup events.db.backup"
```

## Migration from In-Memory

```haskell
-- Development: in-memory
development :: IO ()
development = withSqlitePool ":memory:" 1 $ \pool -> ...

-- Production: file-based
production :: IO ()
production = withSqlitePool "events.db" 1 $ \pool -> ...
```

## Example: Complete CLI Application

```haskell
import Eventium.Store.Sqlite
import System.Directory (getAppUserDataDirectory)

main :: IO ()
main = do
  -- Store in application data directory
  dataDir <- getAppUserDataDirectory "myapp"
  createDirectoryIfMissing True dataDir
  
  let dbPath = dataDir </> "events.db"
  
  withSqlitePool dbPath 1 $ \pool -> do
    -- Initialize on first run
    flip runSqlPool pool $ do
      rawExecute "PRAGMA journal_mode=WAL;" []
      runMigration migrateAll
    
    -- Run application
    runApp pool
```

## Tools

### SQLite CLI
```bash
# Open database
sqlite3 events.db

# Inspect schema
.schema

# Query events
SELECT * FROM events ORDER BY version DESC LIMIT 10;

# Check database size
.dbinfo
```

## Limitations

- **Single Writer** - Only one process should write at a time
- **File Locking** - May have issues on network filesystems
- **Database Size** - Practical limit around 100GB-1TB
- **Concurrent Writes** - Limited compared to PostgreSQL

## Documentation

- [Main README](../README.md) - Project overview
- [SQL Common](../eventium-sql-common/) - Shared SQL utilities
- [Design Documentation](../DESIGN.md) - Architecture details
- [Examples](../examples/) - Bank and cafe examples use SQLite

## License

MIT - see [LICENSE.md](LICENSE.md)
