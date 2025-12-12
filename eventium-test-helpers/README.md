# Eventium Test Helpers

Common testing utilities and helper functions for Eventium-based applications.

## Overview

`eventium-test-helpers` provides shared testing infrastructure used across the Eventium test suite. It includes utilities for event store testing, serialization testing, and common test fixtures that reduce code duplication and provide a consistent testing interface.

## Purpose

This package helps you write better tests for event-sourced applications by providing:
- Reusable test utilities
- Common test fixtures
- Helper functions for assertions
- Shared test patterns

## Features

- ✅ **Event Store Testing** - Utilities for testing event store implementations
- ✅ **Serialization Testing** - Helpers for JSON serialization tests
- ✅ **Test Fixtures** - Common test data and scenarios
- ✅ **Assertion Helpers** - Domain-specific test assertions
- ✅ **HSpec Integration** - Works seamlessly with HSpec test framework

## Installation

Add to your test dependencies in `package.yaml`:

```yaml
tests:
  spec:
    dependencies:
      - eventium-core
      - eventium-test-helpers
      - hspec
      - HUnit
```

Or in your `.cabal` file:

```cabal
test-suite spec
  build-depends:
      eventium-core
    , eventium-test-helpers
    , hspec
    , HUnit
```

## Usage

### Basic Test Setup

```haskell
import Eventium.TestHelpers
import Test.Hspec

spec :: Spec
spec = describe "My Event Store" $ do
  it "stores and retrieves events" $ do
    -- Use test helpers
    result <- testEventStoreRoundTrip myStore events
    result `shouldBe` events
```

### Testing Event Stores

```haskell
import Eventium.TestHelpers

-- Test that events can be written and read back
testEventStoreSpec :: EventStore -> Spec
testEventStoreSpec store = do
  describe "Event Store" $ do
    it "handles empty streams" $
      testEmptyStream store
    
    it "preserves event order" $
      testEventOrdering store
    
    it "enforces optimistic concurrency" $
      testConcurrencyControl store
```

### Testing Serialization

```haskell
import Eventium.TestHelpers

spec :: Spec
spec = describe "Event Serialization" $ do
  it "round-trips through JSON" $ do
    let event = MyEvent "data"
    testSerializationRoundTrip mySerializer event
```

### Common Test Fixtures

```haskell
import Eventium.TestHelpers

-- Use predefined test data
testWithSampleEvents :: IO ()
testWithSampleEvents = do
  let events = sampleEvents
  -- Run tests with sample data
```

## What's Included

### Test Utilities
- **Event Store Testing** - Verify store implementations
- **Serialization Testing** - JSON round-trip tests
- **Projection Testing** - Test projection logic
- **Command Handler Testing** - Verify command handling

### Helper Functions
- **Assertion Helpers** - Domain-specific assertions
- **Mock Builders** - Create test doubles
- **Data Generators** - Generate test data
- **Setup/Teardown** - Common test lifecycle

### Test Patterns
- **Given-When-Then** - BDD-style test structure
- **Property Testing** - QuickCheck integration patterns
- **Integration Testing** - Multi-component test patterns

## Example Test Suite

```haskell
module MyEventStoreSpec (spec) where

import Eventium.TestHelpers
import Test.Hspec

spec :: Spec
spec = describe "My Event Store Implementation" $ do
  
  describe "Basic Operations" $ do
    it "writes events" $ do
      store <- createTestStore
      result <- writeTestEvents store sampleEvents
      result `shouldSatisfy` isSuccess
    
    it "reads events back" $ do
      store <- createTestStore
      writeTestEvents store sampleEvents
      events <- readTestEvents store
      events `shouldBe` sampleEvents
  
  describe "Concurrency" $ do
    it "detects version conflicts" $ do
      store <- createTestStore
      testConcurrencyConflict store `shouldThrow` versionConflictError
```

## Best Practices

### 1. Use Test Helpers Consistently
```haskell
-- Good: Use provided helpers
testEventStoreRoundTrip store events

-- Avoid: Reimplementing test logic
-- (write custom test code)
```

### 2. Leverage Fixtures
```haskell
-- Good: Reuse common test data
let events = sampleBankEvents

-- Avoid: Creating test data in every test
-- let events = [Event1, Event2, ...]
```

### 3. Test Isolation
```haskell
-- Good: Fresh store per test
beforeEach $ createTestStore

-- Avoid: Shared mutable state
-- globalStore <- createTestStore
```

## Testing Different Backends

Test your application against multiple storage backends:

```haskell
import Eventium.Store.Memory (newEventStore)
import Eventium.Store.Sqlite (makeSqliteEventStore)

spec :: Spec
spec = do
  describe "With Memory Store" $
    testEventStoreSpec memoryStore
  
  describe "With SQLite Store" $
    testEventStoreSpec sqliteStore
```

## Integration with HSpec

This package is designed to work with HSpec:

```haskell
-- tests/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- tests/MyModuleSpec.hs
module MyModuleSpec (spec) where
import Eventium.TestHelpers
import Test.Hspec

spec :: Spec
spec = -- your tests
```

## Documentation

- [Main README](../README.md) - Project overview
- [Core Package](../eventium-core/) - Core abstractions
- [Examples](../examples/) - See example test suites

## Used By

All Eventium packages use these test helpers:
- `eventium-core` - Core functionality tests
- `eventium-memory` - Memory store tests
- `eventium-sqlite` - SQLite store tests
- `eventium-postgresql` - PostgreSQL store tests
- `examples-bank` - Bank example tests

## License

MIT - see [LICENSE.md](LICENSE.md)

