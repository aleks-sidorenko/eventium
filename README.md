# Eventium

A Haskell library for building event-sourced applications with CQRS.

## Overview

Eventium provides composable, type-safe abstractions for event sourcing: event stores with optimistic concurrency, pure projections, command handlers, process managers, event subscriptions, and pluggable storage backends. It is a modernized fork of `eventful`, updated for GHC 9.6.

## Packages

| Package | Description |
|---------|-------------|
| **eventium-core** | Core abstractions: event stores, projections, command handlers, process managers, codecs, TH utilities |
| **eventium-memory** | STM-based in-memory event store for development and testing |
| **eventium-sqlite** | SQLite backend via `persistent` |
| **eventium-postgresql** | PostgreSQL backend via `persistent` |
| **eventium-sql-common** | Shared Persistent entity definitions and SQL operations |
| **eventium-testkit** | Shared hspec test utilities |

## Quick Start

```bash
# Enter dev environment (requires Nix with flakes)
nix develop

# Build everything
just build

# Run all tests
just test
```

### Using Eventium in your project

Add `eventium-core` plus a storage backend to your dependencies:

```yaml
dependencies:
  - eventium-core >= 0.1.0
  - eventium-sqlite >= 0.1.0   # or eventium-postgresql, eventium-memory
```

### Minimal example

```haskell
import Eventium
import Eventium.Store.Memory

main :: IO ()
main = do
  tvar <- eventMapTVar
  let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar)
      reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)

  -- Apply a command through a command handler
  result <- applyCommandHandler writer reader myCommandHandler aggregateId myCommand
  case result of
    Left err -> print err
    Right events -> print events
```

## Key Abstractions

- **Projection** -- Pure fold: seed state + event handler. Rebuilds aggregate or read-model state from events.
- **CommandHandler** -- Validates a command against current state and produces events or a domain error.
- **EventStoreReader / EventStoreWriter** -- Polymorphic over key, position, monad, and event types. Supports versioned (per-aggregate) and global (cross-aggregate) streams.
- **ProcessManager** -- Coordinates long-running workflows across aggregates. Reacts to events with pure `[ProcessManagerEffect]` values.
- **EventHandler** -- Composable event consumer with `Contravariant`, `Semigroup`, and `Monoid` instances.
- **EventPublisher** -- Decouples post-write notification from the store writer. `publishingEventStoreWriter` wraps a writer to auto-dispatch after each write.
- **EventSubscription** -- Push-based event delivery. `pollingSubscription` polls the global stream at a configurable interval.
- **Codec** -- Bidirectional event encoding/decoding with JSON support and TH-generated sum-type codecs.

## Examples

Three working examples demonstrate increasing complexity:

### Counter CLI (`examples/counter-cli/`)
Minimal single-file example: bounded counter with in-memory store.
```bash
cabal run counter-cli
```

### Cafe (`examples/cafe/`)
Restaurant ordering system (inspired by [Edument's CQRS tutorial](http://cqrs.nu/tutorial)): tab management, chef todo list as a polling read model.
```bash
cabal run cafe-main -- --help
cabal run cafe-chef-todo-main -- --database-path cafe.db
```

### Bank (`examples/bank/`)
Full CQRS application: accounts, customers, money transfers via process manager, read models, event publishing.
```bash
cabal run bank-main -- --help
```

## Build System

Nix + Cabal with GHC 9.6.7. Cabal files are generated from `package.yaml` via hpack.

```bash
just build          # cabal build all
just test           # cabal test all
just hpack          # regenerate .cabal from package.yaml
just format         # ormolu formatting
just ghcid          # continuous compilation
```

PostgreSQL tests require a running instance (`docker compose up -d`). See [CLAUDE.md](CLAUDE.md) for env var details.

## Documentation

- [Design](DESIGN.md) -- Architecture and design decisions
- [Changelog](CHANGELOG.md) -- Version history
- [Examples](examples/) -- Working applications

## License

MIT -- see [LICENSE.md](LICENSE.md).
