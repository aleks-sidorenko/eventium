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
-- Returns IssueCommand values
```

`runProcessManagerEffects` dispatches the resulting commands.

### Event Publishing

The bank uses `publishingEventStoreWriter` to auto-dispatch events after
each write:

```haskell
cliEventStoreWriter =
  publishingEventStoreWriter writer (synchronousPublisher eventHandler)
```

The event handler prints events and triggers the transfer process manager.

### Read Models

`CustomerAccounts` -- denormalized view matching customers to their accounts.
Handles `StreamEvent` with 4-field pattern matching:

```haskell
handleEvent accounts (StreamEvent uuid _ _ (CustomerCreatedEvent ...)) = ...
```

## Architecture

```
Commands -> CommandHandler -> events table (SQLite)
                                   |
                          publishingEventStoreWriter
                                   |
                     EventHandler (fan-out via <>)
                           |              |
                     Print events    TransferManager
                                    (ProcessManager)
                                           |
                                 runProcessManagerEffects
                                         |
                                    IssueCommand
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
      CustomerAccounts.hs -- denormalized read model
    CLI/
      Options.hs         -- optparse-applicative
      RunCommand.hs      -- command dispatch
      Store.hs           -- event store setup, publishing, process manager wiring
    Models.hs            -- combined BankEvent/BankCommand, TH-generated codecs
    CLI.hs               -- top-level CLI
    Json.hs              -- JSON helpers
  tests/Bank/Models/
    AccountSpec.hs       -- command handler tests using commandHandlerDecide
```

## Testing

```bash
cabal test examples-bank
```

Tests exercise `commandHandlerDecide` directly:

```haskell
commandHandlerDecide accountCommandHandler stateAfterDeposit
  (DebitAccountAccountCommand (DebitAccount 9 "ref"))
  `shouldBe` Right [AccountDebitRejectedAccountEvent $ AccountDebitRejected 4]
```

## Next Steps

- [Counter CLI](../counter-cli/) -- simplest starting point
- [Cafe Example](../cafe/) -- polling subscription, read model
- [Design](../../DESIGN.md)

## License

MIT -- see [LICENSE.md](../../LICENSE.md)
