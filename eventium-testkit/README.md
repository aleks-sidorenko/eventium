# Eventium Testkit

Shared hspec testing utilities for Eventium packages.

## Overview

`eventium-testkit` provides reusable test infrastructure used across
the Eventium test suite: common event types, serialization round-trip helpers,
and event store behavioral tests.

## Installation

Add to your test dependencies:

```yaml
tests:
  spec:
    dependencies:
      - eventium-core
      - eventium-testkit
      - hspec
```

## Usage

The package is used internally by `eventium-memory`, `eventium-sqlite`,
`eventium-postgresql`, and `examples-bank` test suites. Import the helpers
and use them with your backend-specific store setup.

## Documentation

- [Main README](../README.md)
- [Core Package](../eventium-core/)

## License

MIT -- see [LICENSE.md](LICENSE.md)
