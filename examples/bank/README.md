# Bank Example

Full CQRS application: accounts, customers, money transfers, process managers, and event publishing.

## Running

```bash
cabal run bank-main -- --help
```

## Features

### Domain Aggregates

**Account** -- open, deposit, withdraw, close, start/accept/reject transfers.
**Customer** -- create, track associated accounts.

Both use `CommandHandler` with `Void` error type (all validations produce events).

### Transfer Process Manager

Coordinates money transfers across two account aggregates:

```haskell
transferProcessManager :: ProcessManager TransferManager BankEvent BankCommand
```

The `reactTransfer` function is pure -- it returns `[ProcessManagerEffect]`:

```haskell
reactTransfer :: TransferManager -> VersionedStreamEvent BankEvent
              -> [ProcessManagerEffect BankCommand]
```

Uses `IssueCommandWithCompensation` for the transfer credit step: if the target
account rejects the credit, compensation automatically issues a
`RejectTransferCommand` back to the source account.

### Command Dispatcher

The bank wires its aggregates using `commandHandlerDispatcher` for list-based
command routing:

```haskell
dispatcher =
  commandHandlerDispatcher
    cliEventStoreWriter
    cliEventStoreReader
    [mkAggregateHandler accountBankCommandHandler formatAccountError]
```

`processManagerEventHandler` connects the transfer process manager to the
dispatcher, producing an `EventHandler` that plugs directly into the
`EventPublisher`.

### Event Publishing

The bank uses `publishingEventStoreWriter` to auto-dispatch events after
each write:

```haskell
cliEventStoreWriter =
  publishingEventStoreWriter writer (synchronousPublisher eventHandler)
```

The event handler prints events and triggers the transfer process manager via
`processManagerEventHandler`.

### Read Models

**CustomerAccounts** -- in-memory denormalized view matching customers to their accounts.
Handles `StreamEvent` with 4-field pattern matching:

```haskell
handleEvent accounts (StreamEvent uuid _ _ (CustomerCreatedEvent ...)) = ...
```

**Transfers** -- persistent queryable view using the `ReadModel` abstraction. Tracks
transfer lifecycle (Pending → Completed/Failed) in a SQLite `transfers` table. Demonstrates:

- Persistent entity definition for user-defined schema
- `EventHandler` that writes to SQL on each event
- `CheckpointStore` via `sqliteCheckpointStore` for tracking position
- `rebuildReadModel` for replaying all events into the view
- Query functions (`getTransfersByStatus`) for reading the view

## Architecture

```
Commands -> CommandHandler -> events table (SQLite)
                                   |
                          publishingEventStoreWriter
                                   |
                     EventHandler (fan-out via <>)
                           |              |
                     Print events    processManagerEventHandler
                                    (TransferManager)
                                           |
                                  CommandDispatcher
                                 (commandHandlerDispatcher)
                                           |
                              IssueCommand / IssueCommandWithCompensation
```

## Code Structure

```
examples/bank/
  executables/bank-main.hs
  src/Bank/
    Models/
      Account/           -- events, commands, projection, command handler
      Customer/          -- events, commands, projection, command handler
    ProcessManagers/
      TransferManager.hs -- pure process manager with ProcessManagerEffect
    ReadModels/
      CustomerAccounts.hs -- in-memory denormalized read model
      Transfers.hs       -- persistent queryable transfer view (ReadModel)
    CLI/
      Options.hs         -- optparse-applicative
      RunCommand.hs      -- command dispatch
      Store.hs           -- event store setup, publishing, process manager wiring
    Models.hs            -- combined BankEvent/BankCommand, TH-generated codecs
    CLI.hs               -- top-level CLI
    Json.hs              -- JSON helpers
  tests/Bank/
    Models/AccountSpec.hs          -- command handler tests using decide
    ReadModels/TransfersSpec.hs    -- transfer read model lifecycle tests
```

## Testing

```bash
cabal test examples-bank
```

Tests exercise the `decide` function directly via dot syntax:

```haskell
accountCommandHandler.decide stateAfterDeposit
  (DebitAccountAccountCommand (DebitAccount 9 "ref"))
  `shouldBe` Right [AccountDebitRejectedAccountEvent $ AccountDebitRejected 4]
```

## Next Steps

- [Counter CLI](../counter-cli/) -- simplest starting point
- [Cafe Example](../cafe/) -- polling subscription, read model
- [Design](../../DESIGN.md)

## License

MIT -- see [LICENSE.md](../../LICENSE.md)
