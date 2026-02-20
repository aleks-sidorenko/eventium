# Cafe Example

A restaurant ordering system demonstrating workflow patterns, read models, and event subscriptions.

Inspired by [Edument's CQRS tutorial](http://cqrs.nu/tutorial).

## Running

```bash
# Main ordering interface (waiter)
cabal run cafe-main -- --help

# Chef todo list (separate read model, polls event store)
cabal run cafe-chef-todo-main -- --database-path cafe.db
```

## Features

### Tab Aggregate

Single aggregate managing the lifecycle of a restaurant tab:

Open tab -> Place order -> Serve drinks -> Prepare food -> Serve food -> Close tab

Commands are validated against the current tab state:
- Can't serve items that weren't ordered
- Can't serve food before it's prepared
- Can't close a tab with outstanding items
- Must pay the correct amount

### Chef Todo List (Polling Read Model)

A separate executable that uses `pollingSubscription` from `eventium-core` to
build an eventually-consistent read model of outstanding food orders:

```haskell
let checkpoint = CheckpointStore (readIORef seqRef) (writeIORef seqRef)
    globalReader = runEventStoreReaderUsing (`runSqlPool` pool) cliGloballyOrderedEventStore
    sub = pollingSubscription globalReader checkpoint 1000  -- poll every 1000ms
    handler = EventHandler $ \globalEvent -> ...

runSubscription sub handler
```

This demonstrates `EventSubscription`, `CheckpointStore`, and `EventHandler`
working together for cross-process event consumption.

## Architecture

```
Waiter CLI  ->  Tab CommandHandler  ->  events table (SQLite)
                                              |
                          pollingSubscription (1s interval)
                                              |
                                    Chef Todo List (read model)
```

Both executables share the same SQLite database file.

## Code Structure

```
examples/cafe/
  app/
    cafe-main.hs               -- waiter CLI entry point
    cafe-chef-todo-main.hs     -- chef read model entry point
  src/Cafe/
    Models/Tab.hs              -- tab events, commands, projection, command handler
    CLI.hs                     -- main CLI logic
    CLI/Options.hs             -- optparse-applicative definitions
    CLI/Transformer.hs         -- CLI monad (ReaderT over SqlPersistT)
    ChefTodoList.hs            -- polling read model using EventSubscription
    DB.hs                      -- database setup
```

### Key Types

```haskell
-- Command handler with explicit error type
type TabCommandHandler = CommandHandler TabState TabEvent TabCommand TabCommandError

-- Codec wrappers (codec comes first)
codecProjection jsonStringCodec tabProjection
codecCommandHandler jsonStringCodec idCodec tabCommandHandler
```

## Dependencies

- `eventium-core` -- core abstractions
- `eventium-sqlite` -- persistent storage

No `eventium-memory` dependency; the chef todo list uses `pollingSubscription`
with `IORef`-based checkpoint storage.

## Next Steps

- [Counter CLI](../counter-cli/) -- simpler starting point
- [Bank Example](../bank/) -- process manager, event publisher
- [Design](../../DESIGN.md)

## License

MIT -- see [LICENSE.md](../../LICENSE.md)
