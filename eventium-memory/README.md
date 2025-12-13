# Eventium Memory

In-memory implementations of event stores, read models, and projection caches for Eventium.

## Overview

`eventium-memory` provides thread-safe, in-memory storage implementations for the Eventium event sourcing framework. This package is ideal for development, testing, and prototyping event-sourced applications without requiring external dependencies like databases.

## Features

- ✅ **In-Memory Event Store** - Fast, STM-based event storage
- ✅ **Thread-Safe** - Concurrent access via Software Transactional Memory
- ✅ **No External Dependencies** - No database setup required
- ✅ **Perfect for Testing** - Fast test execution with isolated state
- ✅ **Projection Cache** - In-memory snapshot storage
- ✅ **Read Model Support** - Memory-based read model implementation

## Components

### Event Store (`Eventium.Store.Memory`)
Implements both `EventStoreReader` and `EventStoreWriter` with:
- Per-stream versioning
- Global event ordering
- Optimistic concurrency control
- STM transactions for atomicity

### Read Model (`Eventium.ReadModel.Memory`)
In-memory read model implementation for building query-optimized views.

### Projection Cache (`Eventium.ProjectionCache.Memory`)
Stores projection snapshots in memory to avoid replaying entire event histories.

## Usage

```haskell
import Eventium.Store.Memory (newEventStore)
import Control.Concurrent.STM (atomically)

main :: IO ()
main = do
  -- Create a new in-memory event store
  store <- atomically newEventStore
  
  -- Use it with your command handlers and projections
  result <- applyCommandHandler 
    (eventStoreWriter store)
    (eventStoreReader store) 
    myCommandHandler 
    aggregateId 
    command
```

## Installation

Add to your `package.yaml`:

```yaml
dependencies:
  - eventium-core
  - eventium-memory
```

Or to your `.cabal` file:

```cabal
build-depends:
    eventium-core
  , eventium-memory
```

## Use Cases

### Development
Quickly prototype event-sourced applications without database setup.

### Testing
- Fast test execution (no I/O overhead)
- Isolated test state (each test gets fresh store)
- Easy verification of event sequences

### Demonstration
Perfect for demos, tutorials, and learning event sourcing concepts.

## Example

```haskell
-- Create store
store <- atomically newEventStore

-- Write events
writeResult <- atomically $ 
  writeEvents (eventStoreWriter store) 
    streamKey 
    ExpectedPositionAny 
    [event1, event2]

-- Read events back
events <- atomically $ 
  readEvents (eventStoreReader store) 
    streamKey 
    QueryRangeAll
```

## Limitations

- **Not Persistent** - All data lost when process ends
- **Memory Constraints** - Limited by available RAM
- **Single Process** - No distributed access

For production use, consider:
- `eventium-sqlite` - Persistent, single-process storage
- `eventium-postgresql` - Persistent, multi-process storage

## Documentation

- [Main README](../README.md) - Project overview
- [Design Documentation](../DESIGN.md) - Architecture details
- [Examples](../examples/) - Working applications

## License

MIT - see [LICENSE.md](LICENSE.md)
