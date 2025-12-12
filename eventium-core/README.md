# Eventium Core

Core abstractions and utilities for building event sourcing systems in Haskell.

## Overview

`eventium-core` is the foundational package of the Eventium event sourcing framework. It provides all the essential abstractions, interfaces, and utilities needed to build event-sourced applications with CQRS patterns.

## Key Components

### Event Store (`Eventium.Store.Class`)
- **`EventStoreReader`** - Read events from streams (versioned and global)
- **`EventStoreWriter`** - Append events with optimistic concurrency control
- **`StreamEvent`** - Event metadata wrapper with stream keys and positions

### Projection (`Eventium.Projection`)
Pure event fold for rebuilding state from events. Used for both domain aggregates (write model) and read models (query side).

```haskell
data Projection state event = Projection
  { projectionSeed :: state
  , projectionEventHandler :: state -> event -> state
  }
```

### Command Handler (`Eventium.CommandHandler`)
Implements the aggregate pattern from DDD/Event Sourcing. Processes commands, validates against current state, and emits events.

```haskell
data CommandHandler state event command = CommandHandler
  { commandHandlerHandler :: state -> command -> [event]
  , commandHandlerProjection :: Projection state event
  }
```

### Process Manager (`Eventium.ProcessManager`)
Coordinates long-running business processes across multiple aggregates. Implements the Saga pattern for complex workflows.

### Read Model (`Eventium.ReadModel.Class`)
Builds denormalized views optimized for queries. Tracks processed events for eventual consistency with the write side.

### Serializer (`Eventium.Serializer`)
Type-safe event serialization/deserialization with JSON support and Template Haskell utilities for automatic boilerplate generation.

### Template Haskell Utilities (`Eventium.TH`)
- **`deriveJSON`** - Generate JSON instances
- **`deriveSumTypeSerializer`** - Generate serializers for sum types (event polymorphism)
- **`makeProjection`** - Generate projection boilerplate

## Features

- ✅ Type-safe event sourcing abstractions
- ✅ Optimistic concurrency control with `ExpectedPosition`
- ✅ CQRS pattern support (command/query separation)
- ✅ Process Manager (Saga) pattern
- ✅ Projection caching for performance
- ✅ Template Haskell for reducing boilerplate
- ✅ Storage backend agnostic (memory, SQL, NoSQL)

## Usage

Add `eventium-core` to your package dependencies:

```yaml
dependencies:
  - eventium-core
```

Then choose a storage backend:
- `eventium-memory` - In-memory (development/testing)
- `eventium-sqlite` - SQLite (single-process apps)
- `eventium-postgresql` - PostgreSQL (production systems)

## Design Principles

1. **Type Safety** - Phantom types prevent mixing concerns
2. **Functional Purity** - Projections are pure folds
3. **Abstraction** - Backend-agnostic core types
4. **CQRS** - Clear command/query separation
5. **Standard Patterns** - Aggregates, Sagas, Read Models

## Documentation

- [Main README](../README.md) - Project overview
- [Design Documentation](../DESIGN.md) - Detailed architecture
- [Examples](../examples/) - Working applications

## License

MIT - see [LICENSE.md](LICENSE.md)
