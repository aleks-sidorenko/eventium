# Eventium Core

Core abstractions for building event-sourced applications in Haskell.

## Overview

`eventium-core` is the foundational package of the Eventium framework. It defines storage-agnostic interfaces and pure types -- no database drivers, no I/O beyond what the caller provides via monad parameters.

## Key Types

### StreamEvent

```haskell
data StreamEvent key position event = StreamEvent
  { streamEventKey      :: key
  , streamEventPosition :: position
  , streamEventMetadata :: EventMetadata
  , streamEventEvent    :: event
  }
```

Every stored event carries a stream key, a position, metadata (event type, correlation/causation IDs, timestamp), and the domain payload.

### EventStoreReader / EventStoreWriter

```haskell
newtype EventStoreReader key position m event = EventStoreReader
  { getEvents :: QueryRange key position -> m [event] }

newtype EventStoreWriter key position m event = EventStoreWriter
  { storeEvents :: key -> ExpectedPosition position -> [event]
                -> m (Either (EventWriteError position) EventVersion) }
```

Polymorphic over key, position, monad, and event types. `runEventStoreReaderUsing` / `runEventStoreWriterUsing` lift between monads. `codecEventStoreReader` / `codecEventStoreWriter` wrap with a `Codec`.

### Projection

```haskell
data Projection state event = Projection
  { projectionSeed         :: state
  , projectionEventHandler :: state -> event -> state
  }
```

Pure fold for rebuilding state from events. Used for both aggregates (write side) and read models (query side). `getLatestStreamProjection` loads events from a reader and applies them.

### CommandHandler

```haskell
data CommandHandler state event command err = CommandHandler
  { commandHandlerDecide     :: state -> command -> Either err [event]
  , commandHandlerProjection :: Projection state event
  }
```

Validates a command against current state, returning either a domain error or new events. `applyCommandHandler` orchestrates the full load-decide-write cycle.

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

Coordinates cross-aggregate workflows. `react` is pure; `runProcessManagerEffects` dispatches the resulting commands via a `CommandDispatcher`. `IssueCommandWithCompensation` triggers compensation effects when a command fails.

### CommandDispatcher

```haskell
newtype CommandDispatcher m command = CommandDispatcher
  { dispatchCommand :: UUID -> command -> m CommandDispatchResult }

data CommandDispatchResult = CommandSucceeded | CommandFailed Text
```

Routes commands to aggregates and reports outcomes. Construct with `mkCommandDispatcher`, or use `fireAndForgetDispatcher` for legacy callbacks. `commandHandlerDispatcher` (from `Eventium.CommandDispatcher`) builds a dispatcher from a list of `AggregateHandler`s for multi-aggregate routing.

`processManagerEventHandler` wires a `ProcessManager` to a global reader and dispatcher, producing a ready-to-use `EventHandler`.

### EventHandler / EventPublisher / EventSubscription

- **EventHandler** -- composable event consumer (`Contravariant`, `Semigroup`, `Monoid`).
- **EventPublisher** -- decouples post-write dispatch. `publishingEventStoreWriter` wraps a writer for auto-publish.
- **EventSubscription** -- push-based delivery. `pollingSubscription` polls the global stream with a `CheckpointStore`.

### Codec

```haskell
data Codec a b = Codec
  { encode :: a -> b
  , decode :: b -> Maybe a
  }
```

Value-level bidirectional conversion. Composable via `composeCodecs`. `Eventium.TH` generates sum-type codecs and JSON instances.

### TypeEmbedding

```haskell
data TypeEmbedding a b = TypeEmbedding
  { embed   :: a -> b
  , extract :: b -> Maybe a
  }
```

Embeds one sum type into another (e.g. aggregate events into an application-wide event type). Separate from `Codec` to distinguish type-level subset relationships from wire-format encoding.

## Modules

| Module | Purpose |
|--------|---------|
| `Eventium.Store.Class` | Reader/Writer interfaces, monad lifting, codec wrappers |
| `Eventium.Store.Types` | `StreamEvent`, `EventMetadata`, `EventVersion`, `SequenceNumber`, `ExpectedPosition` |
| `Eventium.Store.Queries` | `QueryRange` builders (`allEvents`, `eventsStartingAt`, etc.) |
| `Eventium.Projection` | `Projection`, `StreamProjection`, `getLatestStreamProjection` |
| `Eventium.CommandHandler` | `CommandHandler`, `applyCommandHandler` |
| `Eventium.ProcessManager` | `ProcessManager`, `ProcessManagerEffect`, `CommandDispatcher`, `CommandDispatchResult`, `runProcessManagerEffects`, `processManagerEventHandler` |
| `Eventium.CommandDispatcher` | `AggregateHandler`, `mkAggregateHandler`, `commandHandlerDispatcher` |
| `Eventium.EventHandler` | `EventHandler`, `handleEvents` |
| `Eventium.EventPublisher` | `EventPublisher`, `publishingEventStoreWriter`, `synchronousPublisher` |
| `Eventium.EventSubscription` | `EventSubscription`, `pollingSubscription`, `CheckpointStore` |
| `Eventium.Codec` | `Codec`, `jsonCodec`, `jsonTextCodec`, `composeCodecs` |
| `Eventium.UUID` | UUID utilities (`uuidNextRandom`, `uuidFromText`, `uuidFromInteger`) |
| `Eventium.TH` | Template Haskell: `deriveJSON`, `mkSumTypeCodec`, `mkSumTypeEmbedding`, `makeProjection` |
| `Eventium.ProjectionCache.Types` | Projection cache interface |

## Usage

```yaml
dependencies:
  - eventium-core >= 0.1.0
```

Then pick a storage backend: `eventium-memory`, `eventium-sqlite`, or `eventium-postgresql`.

## Documentation

- [Main README](../README.md)
- [Design](../DESIGN.md)
- [Examples](../examples/)

## License

MIT -- see [LICENSE.md](LICENSE.md)
