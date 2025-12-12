# Eventium

A library for building event sourcing systems in Haskell.

## Project Status

🎉 **Active Development** - Version 0.1.0

Eventium has been recently updated with significant improvements:
- ✅ **Template Haskell Support Restored** - Full TH functionality with GHC 9.6.7
- ✅ **Modern Build System** - Nix + Cabal for reproducible builds
- ✅ **Comprehensive Module Suite** - Core, memory, SQL, and NoSQL implementations
- ✅ **Working Examples** - Bank, cafe, and counter applications
- ✅ **Test Coverage** - Full test suite with helper utilities

## Architecture

Eventium provides a complete event sourcing toolkit with the following modules:

### Core Modules
- **`eventium-core`** - Core event sourcing abstractions and Template Haskell utilities
- **`eventium-test-helpers`** - Testing utilities and helpers

### Storage Implementations
- **`eventium-memory`** - In-memory event store for development and testing
- **`eventium-postgresql`** - PostgreSQL-based persistent event store
- **`eventium-sqlite`** - SQLite-based persistent event store
- **`eventium-sql-common`** - Shared utilities for SQL-based stores

## Build System

This project uses **Nix + Cabal** for reproducible builds with GHC 9.6.7.

### Quick Start

```bash
# Enter development environment
nix develop

# Build all packages
cabal build all

# Run tests
cabal test eventium-core eventium-memory eventium-postgresql eventium-sqlite
```

### Automated Build & Test

Use the provided script for a complete build and test cycle:

```bash
# Run comprehensive build and test
./scripts/test-build.sh
```

This script will:
- Enter the Nix development environment
- Regenerate cabal files from package.yaml
- Build core packages
- Run all tests
- Provide a summary of applied fixes

### Available Commands (in nix develop)

- `cabal build` - Build all packages
- `cabal test` - Run all tests
- `hpack` - Generate cabal files from package.yaml
- `ghcid` - Continuous compilation

### Database Tools

- `psql` - PostgreSQL client
- `sqlite3` - SQLite client

## Examples

The project includes several working examples demonstrating different patterns:

### 🏦 Bank Example (`examples/bank/`)
A complete banking application showcasing:
- Account management with command handlers
- Customer operations
- Transfer process managers
- Read models for customer accounts

```bash
# Build and run the bank example
cabal build examples-bank
cabal run bank-main -- --help
```

### ☕ Cafe Example (`examples/cafe/`)
A cafe ordering system (inspired by [Edument's CQRS tutorial](http://cqrs.nu/tutorial)):
- Order management
- Chef todo lists
- Event-driven workflow

```bash
# Build and run the cafe example
cabal build examples-cafe
cabal run cafe-main -- --help
```

### 🔢 Counter CLI (`examples/counter-cli/`)
A simple counter application demonstrating basic event sourcing concepts:

```bash
# Build and run the counter example
cabal build examples-counter-cli
cabal run counter-cli
```

## Getting Started

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd eventium
   ```

2. **Enter development environment**:
   ```bash
   nix develop
   ```

3. **Build and test**:
   ```bash
   ./scripts/test-build.sh
   ```

4. **Explore examples**:
   ```bash
   # Try the bank example
   cabal run bank-main -- create-account --customer-id "cust-123" --account-id "acc-456"
   
   # Try the cafe example  
   cabal run cafe-main -- --help
   ```

## Documentation
- **[Design](DESIGN.md)** - Design summary and main concepts
- **[Changelog](CHANGELOG.md)** - Complete version history
- **[Examples](examples/)** - Working example applications

## Contributing

This project is under active development. The current focus areas are:

- 📚 Improving documentation and tutorials
- 🧪 Expanding test coverage
- 🔧 Adding more storage backends
- 📦 Performance optimizations

## License

MIT - see [LICENSE.md](LICENSE.md) for details.
