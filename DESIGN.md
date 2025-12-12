# Eventium Design Documentation

## Overview

**Eventium** is a Haskell library for building event sourcing and CQRS systems. It provides a complete, type-safe toolkit with clear separation of concerns.

## Core Abstractions

### 1. Event Store (`Eventium.Store.Class`)

**Intention**: Persist and retrieve events from streams

- **`EventStoreReader`** - Queries events from a stream by key and position
  - `VersionedEventStoreReader` - reads from UUID-keyed streams with event versions
  - `GlobalEventStoreReader` - reads from global ordered event stream with sequence numbers
  
- **`EventStoreWriter`** - Appends events to streams with optimistic concurrency control
  - Uses `ExpectedPosition` for optimistic locking (prevents concurrent write conflicts)
  - Returns `EventWriteError` on version conflicts
  
- **`StreamEvent`** - Wraps events with their stream key and position metadata

**Key Concepts**:
- `EventVersion` - per-stream ordering (strictly increasing per stream)
- `SequenceNumber` - global ordering across all streams
- `QueryRange` - flexible event querying (all, range, limit)

---

### 2. Projection (`Eventium.Projection`)

**Intention**: Rebuild state from events (read-side materialization)

```haskell
data Projection state event = Projection
  { projectionSeed :: state
  , projectionEventHandler :: state -> event -> state
  }
```

- Pure fold over events: `state = fold handler seed events`
- **`StreamProjection`** - tracks which position the projection is caught up to
- Enables snapshotting and incremental updates

**Usage**: Both for domain aggregates (write model) and read models (queries)

---

### 3. Command Handler (`Eventium.CommandHandler`)

**Intention**: Process commands and emit events (write-side)

```haskell
data CommandHandler state event command = CommandHandler
  { commandHandlerHandler :: state -> command -> [event]
  , commandHandlerProjection :: Projection state event
  }
```

**Pattern**:
1. Load current aggregate state via projection
2. Validate command against current state
3. Emit events if valid
4. Store events with optimistic concurrency check
5. Update aggregate state

This implements the **aggregate pattern** from DDD/Event Sourcing.

---

### 4. Process Manager (`Eventium.ProcessManager`)

**Intention**: Coordinate long-running business processes across aggregates

```haskell
data ProcessManager state event command = ProcessManager
  { processManagerProjection :: Projection state (VersionedStreamEvent event)
  , processManagerPendingCommands :: state -> [ProcessManagerCommand event command]
  , processManagerPendingEvents :: state -> [StreamEvent UUID () event]
  }
```

**Pattern**:
- Listens to events from multiple streams
- Maintains internal state (saga state)
- Emits commands to other aggregates or events to streams
- Implements the **Saga/Process Manager** pattern

---

### 5. Event Bus (`Eventium.EventBus`)

**Intention**: Publish events to handlers after storage

- **`synchronousEventBusWrapper`** - decorates event store to publish after write
- Enables event handlers (read models, process managers) to react to events
- Simple in-process pub/sub (depth-first handler execution)

---

### 6. Read Model (`Eventium.ReadModel.Class`)

**Intention**: Build materialized views optimized for queries

```haskell
data ReadModel model serialized m = ReadModel
  { readModelModel :: model
  , readModelLatestAppliedSequence :: model -> m SequenceNumber
  , readModelHandleEvents :: model -> [GlobalStreamEvent serialized] -> m ()
  }
```

**Pattern**:
- Polls global event stream for new events
- Updates denormalized query model
- Tracks last processed sequence number
- Eventually consistent with write side

---

### 7. Projection Cache (`Eventium.ProjectionCache.Types`)

**Intention**: Performance optimization via snapshotting

- Stores projection state snapshots at specific versions
- Avoids replaying entire event history
- Key-value store abstraction for snapshots

---

### 8. Serializer (`Eventium.Serializer`)

**Intention**: Type-safe event serialization/deserialization

```haskell
data Serializer a b = Serializer
  { serialize :: a -> b
  , deserialize :: b -> Maybe a
  , deserializeEither :: b -> Either String a
  }
```

**Features**:
- Composable serializers
- JSON support (`jsonSerializer`, `jsonTextSerializer`)
- Sum type handling for event polymorphism
- Template Haskell support for boilerplate generation

---

## Design Principles

### 1. Type Safety
Heavy use of phantom types and newtypes to prevent mixing concerns (e.g., `EventVersion` vs `SequenceNumber`)

### 2. Abstraction
Core types are polymorphic over storage backend (memory, PostgreSQL, SQLite)

### 3. Functional Purity
Projections are pure folds; side effects isolated to stores/handlers

### 4. CQRS
Clear separation between command side (CommandHandler) and query side (ReadModel/Projection)

### 5. Optimistic Concurrency
`ExpectedPosition` prevents lost updates without distributed locks

### 6. Event Sourcing Patterns
Implements standard patterns (Aggregates, Sagas, Read Models)

---

## Architecture Flow

```
Command → CommandHandler → Events → EventStore
                ↓
         [Event Bus publishes]
                ↓
    ┌───────────┴───────────┐
    ↓                       ↓
ReadModel              ProcessManager
(queries)          (cross-aggregate logic)
                          ↓
                   New Commands
```

---

## Module Organization

### Core Modules
- **`eventium-core`** - Core event sourcing abstractions and Template Haskell utilities
- **`eventium-test-helpers`** - Testing utilities and helpers

### Storage Implementations
- **`eventium-memory`** - In-memory event store for development and testing
- **`eventium-postgresql`** - PostgreSQL-based persistent event store
- **`eventium-sqlite`** - SQLite-based persistent event store
- **`eventium-sql-common`** - Shared utilities for SQL-based stores

---

## Usage Patterns

### Basic Event Sourcing Flow

1. **Define Events and State**
   ```haskell
   data AccountEvent = AccountOpened | MoneyDeposited Amount
   data AccountState = Account { balance :: Amount }
   ```

2. **Create a Projection**
   ```haskell
   accountProjection :: Projection AccountState AccountEvent
   accountProjection = Projection initialState eventHandler
   ```

3. **Define Commands and Handler**
   ```haskell
   data AccountCommand = OpenAccount | Deposit Amount
   
   accountHandler :: CommandHandler AccountState AccountEvent AccountCommand
   accountHandler = CommandHandler handler accountProjection
   ```

4. **Apply Commands**
   ```haskell
   applyCommandHandler writer reader accountHandler uuid command
   ```

### Process Manager Pattern

Used for cross-aggregate workflows (e.g., money transfers between accounts):

```haskell
transferProcessManager :: ProcessManager TransferState BankEvent BankCommand
transferProcessManager = ProcessManager projection pendingCommands pendingEvents
```

### Read Model Pattern

For optimized queries:

```haskell
accountListReadModel :: ReadModel AccountListDB BankEvent IO
accountListReadModel = ReadModel db getLatestSeq handleEvents
```

---

## Benefits

- **Type Safety**: Compile-time guarantees for event handling
- **Testability**: Pure projections and command handlers are easy to test
- **Flexibility**: Pluggable storage backends
- **Performance**: Projection caching for large event streams
- **Scalability**: CQRS enables independent scaling of read/write sides
- **Audit Trail**: Complete history of all state changes
- **Time Travel**: Replay events to any point in time

