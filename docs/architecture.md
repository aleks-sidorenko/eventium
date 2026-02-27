# Eventium Architecture & Design

## Core Concepts

### Event Sourcing

State is not stored directly. Instead, every state change is captured as an
immutable **event**. Current state is reconstructed by folding (replaying) the
event history through a **projection**.

### CQRS

Commands (writes) and queries (reads) follow separate paths:

- **Write side**: `CommandHandler` validates a command against the current
  projection state and produces new events.
- **Read side**: Projections, read models, and subscriptions consume events to
  build query-optimized views.

## Data Model

### StreamEvent

Every persisted event is wrapped in a `StreamEvent`:

```haskell
data StreamEvent key position event = StreamEvent
  { streamEventKey      :: key
  , streamEventPosition :: position
  , streamEventMetadata :: EventMetadata
  , streamEventPayload  :: event
  }
```

- **key** -- identifies the stream (typically `UUID` for aggregates, `()` for the global stream).
- **position** -- ordering within the stream (`EventVersion` per aggregate, `SequenceNumber` globally).
- **metadata** -- `EventMetadata` carrying event type name, optional correlation/causation IDs, and timestamp.
- **event** -- the domain payload.

Common type aliases:

```haskell
type VersionedStreamEvent event = StreamEvent UUID EventVersion event
type GlobalStreamEvent event    = StreamEvent () SequenceNumber (VersionedStreamEvent event)
```

The global stream wraps versioned events, giving each one a cross-aggregate
sequence number while preserving the original stream key and version.

### Storage Schema (SQL backends)

A single `events` table stores both per-aggregate and global ordering:

| Column           | Type               | Purpose                           |
| ---------------- | ------------------ | --------------------------------- |
| `id` (PK)        | auto-increment     | Global sequence number            |
| `uuid`           | UUID               | Aggregate / stream identifier     |
| `version`        | Int                | Position within the stream        |
| `payload`        | JSON/JSONB         | Serialized event payload          |
| `metadata`       | JSONB (nullable)   | EventMetadata (event type, correlation/causation IDs, timestamp) |

A unique constraint on `(uuid, version)` enforces per-stream ordering.
The auto-increment primary key provides global ordering without a separate table.

PostgreSQL uses `LOCK IN EXCLUSIVE MODE` during writes to guarantee that
auto-increment IDs are assigned in commit order, preventing gaps visible to
concurrent readers. SQLite relies on its single-writer serialization.

## Abstractions

### Event Store

```haskell
newtype EventStoreReader key position m event = EventStoreReader
  { getEvents :: QueryRange key position -> m [event] }

newtype EventStoreWriter key position m event = EventStoreWriter
  { storeEvents :: key -> ExpectedPosition position -> [event]
                -> m (Either (EventWriteError position) EventVersion) }
```

Polymorphic over key type, position type, monad, and event type.
`runEventStoreReaderUsing` / `runEventStoreWriterUsing` lift stores between
monads (e.g. `STM` to `IO`, `SqlPersistT m` to `m`).

**Optimistic concurrency** is enforced via `ExpectedPosition`:

- `AnyPosition` -- no check
- `NoStream` -- stream must not exist
- `StreamExists` -- stream must already exist
- `ExactPosition v` -- stream must be at exactly version `v`

### Projection

```haskell
data Projection state event = Projection
  { projectionSeed         :: state
  , projectionEventHandler :: state -> event -> state
  }
```

A pure fold. Used for both write-side aggregates and read-side models.
`getLatestStreamProjection` loads events from a store and folds them.

### CommandHandler

```haskell
data CommandHandler state event command err = CommandHandler
  { commandHandlerDecide     :: state -> command -> Either err [event]
  , commandHandlerProjection :: Projection state event
  }
```

Implements the aggregate pattern. `commandHandlerDecide` validates business
rules against the current state and returns either a domain error or new events.
`applyCommandHandler` orchestrates the full cycle: load projection, decide,
write events.

### ProcessManager

```haskell
data ProcessManager state event command = ProcessManager
  { processManagerProjection :: Projection state (VersionedStreamEvent event)
  , processManagerReact      :: state -> VersionedStreamEvent event
                             -> [ProcessManagerEffect command]
  }

data ProcessManagerEffect command
  = IssueCommand UUID command
  | IssueCommandWithCompensation UUID command (Text -> [ProcessManagerEffect command])
```

Coordinates workflows across aggregates. The `react` function is pure -- it
returns data describing what commands to issue, not monadic actions.

`IssueCommandWithCompensation` extends simple command dispatch with a
compensation handler: if the dispatched command fails, the compensation
function receives the failure reason and produces follow-up effects (e.g.
rejecting a transfer when the target account refuses a credit).

### CommandDispatcher

```haskell
newtype CommandDispatcher m command = CommandDispatcher
  { dispatchCommand :: UUID -> command -> m CommandDispatchResult }

data CommandDispatchResult
  = CommandSucceeded
  | CommandFailed Text
```

Replaces bare `UUID -> command -> m ()` dispatch functions.
`CommandDispatchResult` makes failure observable so that process managers can
react with compensation effects.

- `mkCommandDispatcher` -- construct from a dispatch function.
- `fireAndForgetDispatcher` -- adapt a legacy callback that ignores outcomes.
- `commandHandlerDispatcher` -- route commands through a list of
  `AggregateHandler`s (existential wrappers that erase aggregate state and
  error types).

`processManagerEventHandler` wires a `ProcessManager` to a
`GlobalEventStoreReader` and a `CommandDispatcher`, producing an `EventHandler`
suitable for use with `EventPublisher`.

`runProcessManagerEffects` executes `[ProcessManagerEffect]` via a
`CommandDispatcher`, triggering compensation handlers on `CommandFailed`.

### EventHandler

```haskell
newtype EventHandler m event = EventHandler { handleEvent :: event -> m () }
```

Composable event consumer. `Semigroup` combines handlers (fan-out),
`Contravariant` maps over the event type.

### EventPublisher

```haskell
publishingEventStoreWriter
  :: (Monad m)
  => VersionedEventStoreWriter m event
  -> EventPublisher m event
  -> VersionedEventStoreWriter m event
```

Wraps a store writer so that after each successful write, events are
dispatched to an `EventPublisher`. `synchronousPublisher` creates a publisher
from an `EventHandler` for in-process dispatch.

### EventSubscription

```haskell
pollingSubscription
  :: (MonadIO m)
  => GlobalEventStoreReader m event
  -> CheckpointStore m SequenceNumber
  -> PollingIntervalMillis
  -> EventSubscription m (GlobalStreamEvent event)
```

Polls the global event stream at a configurable interval. A `CheckpointStore`
tracks the last consumed sequence number so the subscription resumes where it
left off. Suitable for building eventually-consistent read models in separate
processes.

### Codec

```haskell
data Codec a b = Codec
  { encode :: a -> b
  , decode :: b -> Maybe a
  }
```

Bidirectional conversion between domain events and storage representation.
Composable via `composeCodecs`. Template Haskell generates codecs
for sum types (`mkSumTypeCodec`).

### TypeEmbedding

```haskell
data TypeEmbedding a b = TypeEmbedding
  { embed   :: a -> b
  , extract :: b -> Maybe a
  }
```

Structurally identical to `Codec` but carries different semantics: it embeds
one sum type into another (e.g. aggregate events into an application-wide sum
type). Using a separate type prevents accidentally mixing wire-format codecs
with type-level subset relationships.

## Package Structure

```
eventium-core          Pure abstractions (no I/O)
eventium-memory        STM-based in-memory backend
eventium-sql-common    Shared Persistent entities and SQL operations
eventium-postgresql    PostgreSQL backend (uses table locking)
eventium-sqlite        SQLite backend (uses EXCLUSIVE transactions)
eventium-testkit       Shared hspec utilities
examples/counter-cli   Minimal example: bounded counter, in-memory store
examples/cafe          Cafe ordering: tabs, chef todo list (polling subscription)
examples/bank          Banking: accounts, customers, transfers (process manager, publisher)
```

## Design Decisions

### Single events table

Both per-aggregate reads (filter by `uuid`, order by `version`) and global reads
(order by `id`) use the same table. This avoids dual-write consistency issues
and keeps the schema simple.

### Pure process managers

`processManagerReact` returns `[ProcessManagerEffect]` -- plain data values --
rather than performing I/O directly. This makes process manager logic
unit-testable without mocking stores. Compensation logic is also pure:
`IssueCommandWithCompensation` carries a function producing further effects on
failure, keeping the entire saga decision tree in testable data.

### Explicit command handler errors

`CommandHandler` carries an `err` type parameter so domain validation errors
are distinguished from infrastructure errors (concurrency conflicts) at the
type level.

### Codec as record, not typeclass

`Codec` is a value-level record rather than a typeclass. This allows
multiple codecs for the same type and avoids orphan-instance problems.
Codecs compose with `composeCodecs` and work with TH-generated
sum-type boilerplate.

### Metadata on StreamEvent

`EventMetadata` (event type, correlation/causation IDs, timestamp) lives on
every `StreamEvent`. This supports event-type-based routing, distributed
tracing, and audit requirements without requiring the domain event type to
carry infrastructure concerns.
