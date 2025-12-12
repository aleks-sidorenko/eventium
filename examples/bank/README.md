# Bank Example

A complete banking application demonstrating event sourcing patterns with Eventium.

## Overview

The bank example is a comprehensive reference implementation showcasing real-world event sourcing patterns. It demonstrates account management, customer operations, money transfers, and read models—all built with Eventium and persisted using SQLite.

## Features

### Domain Models

#### 🏦 Account Aggregate
- **Open Account** - Create new bank accounts
- **Deposit Money** - Add funds to account
- **Withdraw Money** - Remove funds (with balance validation)
- **Close Account** - Deactivate account
- **Transfer Money** - Move funds between accounts

#### 👤 Customer Aggregate
- **Create Customer** - Register new customers
- **Link Accounts** - Associate accounts with customers
- **Customer Profile** - Manage customer information

### Event Sourcing Patterns

#### Command Handlers
Validates business rules and emits events:
```haskell
accountCommandHandler :: CommandHandler Account AccountEvent AccountCommand
customerCommandHandler :: CommandHandler Customer CustomerEvent CustomerCommand
```

#### Projections
Rebuilds aggregate state from events:
```haskell
accountProjection :: Projection Account AccountEvent
customerProjection :: Projection Customer CustomerEvent
```

#### Process Managers
Coordinates multi-aggregate workflows (e.g., transfers between accounts):
```haskell
transferProcessManager :: ProcessManager TransferState BankEvent BankCommand
```

#### Read Models
Denormalized views for queries:
```haskell
customerAccountsReadModel :: ReadModel CustomerAccountsDB BankEvent IO
```

## Architecture

```
Commands → Command Handlers → Events → Event Store (SQLite)
                                  ↓
                            [Event Bus]
                                  ↓
                      ┌─────────────────────┐
                      ↓                     ↓
              Read Models            Process Managers
           (Customer Accounts)      (Transfer Workflow)
```

## Installation

```bash
# Build the example
cabal build examples-bank

# Or from project root
cabal build all
```

## Usage

### Command-Line Interface

The bank example provides a CLI for interacting with the event-sourced system:

```bash
# Show help
cabal run bank-main -- --help

# Create a customer
cabal run bank-main -- create-customer --name "Alice Smith"

# Open an account
cabal run bank-main -- open-account \
  --customer-id "550e8400-e29b-41d4-a716-446655440000" \
  --initial-balance 1000.0

# View account details
cabal run bank-main -- view-account "660e8400-e29b-41d4-a716-446655440001"

# View all accounts for a customer
cabal run bank-main -- view-customer-accounts "Alice Smith"

# Transfer money between accounts
cabal run bank-main -- transfer \
  --from "660e8400-e29b-41d4-a716-446655440001" \
  --to "660e8400-e29b-41d4-a716-446655440002" \
  --amount 100.0
```

### Database Location

By default, the bank example uses `database.db` in the current directory:

```bash
# Use default database
cabal run bank-main -- create-customer --name "Bob"

# Specify custom database
cabal run bank-main -- --database-path ~/mybank.db create-customer --name "Bob"
```

## Code Structure

```
examples/bank/
├── executables/
│   └── bank-main.hs              # CLI entry point
├── src/
│   └── Bank/
│       ├── CLI/
│       │   ├── Options.hs        # Command-line parsing
│       │   ├── RunCommand.hs     # Command execution
│       │   └── Store.hs          # Event store setup
│       ├── Models/
│       │   ├── Account/          # Account aggregate
│       │   │   ├── Commands.hs   # Account commands
│       │   │   ├── Events.hs     # Account events
│       │   │   ├── Projection.hs # Account state projection
│       │   │   └── CommandHandler.hs
│       │   ├── Customer/         # Customer aggregate
│       │   │   ├── Commands.hs
│       │   │   ├── Events.hs
│       │   │   ├── Projection.hs
│       │   │   └── CommandHandler.hs
│       │   └── Transfer/         # Transfer process manager
│       ├── ReadModels/
│       │   └── CustomerAccounts.hs  # Query-optimized view
│       ├── Models.hs             # Combined domain model
│       ├── CLI.hs                # CLI orchestration
│       └── Json.hs               # JSON serialization
└── tests/
    └── Bank/Models/
        └── AccountSpec.hs        # Account tests
```

## Key Concepts Demonstrated

### 1. Aggregate Pattern
Each aggregate (Account, Customer) maintains its own event stream with optimistic concurrency control.

### 2. Command Validation
Business rules are enforced before events are emitted:
- Can't withdraw more than balance
- Can't operate on closed accounts
- Account must exist to receive transfers

### 3. Event Sourcing
All state changes are captured as events:
- `AccountOpened`
- `MoneyDeposited`
- `MoneyWithdrawn`
- `AccountClosed`
- `CustomerCreated`

### 4. CQRS (Command Query Responsibility Segregation)
- **Write Side**: Command handlers validate and emit events
- **Read Side**: Read models provide optimized queries

### 5. Process Manager (Saga)
Transfer process coordinates multiple aggregates:
1. Debit source account
2. Credit target account
3. Handle failures (compensating transactions)

### 6. Serialization
Events are serialized to JSON for storage:
```haskell
accountEventSerializer :: Serializer AccountEvent BankEvent
```

### 7. Template Haskell
Reduces boilerplate with code generation:
```haskell
mkSumTypeSerializer "accountEventSerializer" ''AccountEvent ''BankEvent
```

## Example Workflow

```bash
# 1. Create two customers
cabal run bank-main -- create-customer --name "Alice"
cabal run bank-main -- create-customer --name "Bob"

# 2. Open accounts for each
cabal run bank-main -- open-account \
  --customer-id <alice-uuid> \
  --initial-balance 1000.0

cabal run bank-main -- open-account \
  --customer-id <bob-uuid> \
  --initial-balance 500.0

# 3. Transfer money from Alice to Bob
cabal run bank-main -- transfer \
  --from <alice-account-uuid> \
  --to <bob-account-uuid> \
  --amount 250.0

# 4. View results
cabal run bank-main -- view-customer-accounts "Alice"
cabal run bank-main -- view-customer-accounts "Bob"
```

## Testing

Run the test suite:

```bash
cabal test examples-bank
```

Tests cover:
- Account command validation
- Event projection logic
- Business rule enforcement
- Serialization round-trips

## Learning Path

1. **Start with Account Model** (`src/Bank/Models/Account/`)
   - Study events, commands, and projection
   - Understand aggregate pattern

2. **Explore Command Handler** (`CommandHandler.hs`)
   - See business rule validation
   - Learn event emission patterns

3. **Check CLI Integration** (`src/Bank/CLI/`)
   - See how commands are executed
   - Understand event store setup

4. **Study Read Models** (`src/Bank/ReadModels/`)
   - Learn query-side patterns
   - See denormalization techniques

5. **Advanced: Process Manager** (`src/Bank/Models/Transfer/`)
   - Understand cross-aggregate coordination
   - Learn saga patterns

## Technologies Used

- **Eventium Core** - Event sourcing framework
- **SQLite** - Persistent event storage
- **Persistent** - Type-safe database access
- **Optparse-Applicative** - CLI parsing
- **Aeson** - JSON serialization
- **Lens** - Functional record updates
- **Template Haskell** - Code generation

## Next Steps

- Explore the [Cafe Example](../cafe/) for different patterns
- Try the [Counter CLI](../counter-cli/) for a simpler introduction
- Read the [Design Documentation](../../DESIGN.md) for architectural details
- Check [Main README](../../README.md) for project overview

## License

MIT - see [LICENSE.md](../../LICENSE.md)

