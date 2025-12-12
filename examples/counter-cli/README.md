# Counter CLI Example

A simple counter application demonstrating the fundamentals of event sourcing with Eventium.

## Overview

The counter CLI is the simplest Eventium example, perfect for learning the basics of event sourcing. It implements a bounded counter (0-100) with increment, decrement, and reset operations, all stored as events in an in-memory event store.

## Features

- **Increment Counter** - Add a value to the counter
- **Decrement Counter** - Subtract a value from the counter
- **Reset Counter** - Set counter back to 0
- **Bounded Range** - Counter stays between 0 and 100
- **Interactive REPL** - Command-line interface for experimentation

## Why Start Here?

This example is ideal for learning because it:
- ✅ **Minimal Complexity** - Single aggregate, simple domain
- ✅ **Core Concepts Only** - Events, commands, projections
- ✅ **No Persistence** - In-memory store keeps it simple
- ✅ **Interactive** - See event sourcing in action immediately
- ✅ **Self-Contained** - Everything in one file

## Installation

```bash
# Build the example
cabal build examples-counter-cli

# Or from project root
cabal build all
```

## Usage

```bash
# Run the counter CLI
cabal run counter-cli
```

### Interactive Session

```
Current state: CounterState {unCounterState = 0}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
IncrementCounter 10
Events generated: [CounterAmountAdded 10]

Current state: CounterState {unCounterState = 10}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
IncrementCounter 25
Events generated: [CounterAmountAdded 25]

Current state: CounterState {unCounterState = 35}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
DecrementCounter 5
Events generated: [CounterAmountAdded (-5)]

Current state: CounterState {unCounterState = 30}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
ResetCounter
Events generated: [CounterAmountAdded (-30)]

Current state: CounterState {unCounterState = 0}
```

### Boundary Validation

```
Current state: CounterState {unCounterState = 95}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
IncrementCounter 10
Events generated: [CounterOutOfBounds 105]

Current state: CounterState {unCounterState = 95}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
DecrementCounter 100
Events generated: [CounterOutOfBounds (-5)]

Current state: CounterState {unCounterState = 95}
```

## Code Structure

The entire example is in a single file: `counter-cli.hs`

### Core Components

#### 1. State
```haskell
newtype CounterState = CounterState { unCounterState :: Int }
```
The current value of the counter.

#### 2. Events
```haskell
data CounterEvent
  = CounterAmountAdded Int      -- Amount was added to counter
  | CounterOutOfBounds Int      -- Attempted invalid operation
```
Past-tense descriptions of what happened.

#### 3. Commands
```haskell
data CounterCommand
  = IncrementCounter Int
  | DecrementCounter Int
  | ResetCounter
```
Intentions to change the counter.

#### 4. Projection
```haskell
counterProjection :: Projection CounterState CounterEvent
counterProjection = Projection
  (CounterState 0)              -- Initial state
  handleCounterEvent            -- Event handler
```
Rebuilds state from events.

#### 5. Command Handler
```haskell
counterCommandHandler :: CommandHandler CounterState CounterEvent CounterCommand
counterCommandHandler = CommandHandler
  handlerCounterCommand         -- Validates and emits events
  counterProjection             -- State projection
```
Validates commands and produces events.

## Key Concepts Demonstrated

### 1. Event Sourcing Basics
State is derived from a sequence of events:
```
Events: [CounterAmountAdded 10, CounterAmountAdded 25, CounterAmountAdded (-5)]
State:  0 → 10 → 35 → 30
```

### 2. Commands vs Events
- **Commands** - Requests that can be rejected
- **Events** - Facts that already happened

### 3. Business Rules
The command handler enforces rules:
- Counter must stay between 0 and 100
- Invalid operations emit `CounterOutOfBounds` (doesn't change state)

### 4. Projection Pattern
State is rebuilt by folding events:
```haskell
handleCounterEvent :: CounterState -> CounterEvent -> CounterState
handleCounterEvent (CounterState k) (CounterAmountAdded x) = 
  CounterState (k + x)
```

### 5. In-Memory Event Store
Uses STM (Software Transactional Memory) for thread-safe storage:
```haskell
tvar <- eventMapTVar
let writer = tvarEventStoreWriter tvar
    reader = tvarEventStoreReader tvar
```

## Learning Path

### Step 1: Run and Experiment
```bash
cabal run counter-cli
# Try different commands
# See how events build state
```

### Step 2: Read the Code
Open `counter-cli.hs` and study:
1. Data types (State, Events, Commands)
2. Projection (how events build state)
3. Command handler (validation logic)
4. Main loop (event store usage)

### Step 3: Modify It
Try adding features:
- New command: `SetCounter n`
- Different bounds: 0-1000
- New event: `CounterReset`
- Validation: Only even numbers

### Step 4: Add Persistence
Replace in-memory store with SQLite:
```haskell
import Eventium.Store.Sqlite

-- Instead of:
tvar <- eventMapTVar

-- Use:
pool <- createSqlitePool ":memory:" 1
initializeSqliteEventStore defaultSqlEventStoreConfig pool
```

## Example Modifications

### Add a SetCounter Command

```haskell
data CounterCommand
  = IncrementCounter Int
  | DecrementCounter Int
  | ResetCounter
  | SetCounter Int              -- New command

handlerCounterCommand (CounterState k) (SetCounter n) =
  if n >= 0 && n <= 100
    then [CounterAmountAdded (n - k)]
    else [CounterOutOfBounds n]
```

### Track Command History

```haskell
data CounterState = CounterState
  { unCounterState :: Int
  , commandHistory :: [CounterCommand]  -- New field
  }
```

## Comparison with Other Examples

| Feature | Counter CLI | Cafe | Bank |
|---------|-------------|------|------|
| **Complexity** | Minimal | Medium | High |
| **Aggregates** | 1 | 1 | 2+ |
| **Storage** | Memory | SQLite | SQLite |
| **Read Models** | ❌ | ✅ | ✅ |
| **Process Manager** | ❌ | ❌ | ✅ |
| **Best For** | Learning basics | Workflows | Complex domains |

## Technologies Used

- **Eventium Core** - Event sourcing framework
- **Eventium Memory** - In-memory event store
- **STM** - Software Transactional Memory
- **Safe** - Safe parsing utilities

## What's Next?

After mastering the counter example:

1. **Add Features** - Extend the counter with new commands
2. **Add Persistence** - Switch to SQLite storage
3. **Try Cafe Example** - Learn workflow patterns
4. **Try Bank Example** - Explore complex domains
5. **Build Your Own** - Create a custom event-sourced app

## Learning Resources

- [Main README](../../README.md) - Project overview
- [Design Documentation](../../DESIGN.md) - Architecture details
- [Cafe Example](../cafe/) - Next step in complexity
- [Bank Example](../bank/) - Advanced patterns

## Tips for Learning

1. **Run First** - Get hands-on experience immediately
2. **Read Code** - It's only ~95 lines, very approachable
3. **Experiment** - Try invalid commands, see what happens
4. **Modify** - Add features to solidify understanding
5. **Ask Questions** - What if counter was unbounded? What if we tracked history?

## Common Questions

### Q: Why emit events instead of just updating state?
**A:** Events provide:
- Complete audit trail
- Ability to rebuild state at any point in time
- Foundation for CQRS and event-driven architectures

### Q: Why use a command handler instead of direct state updates?
**A:** Command handlers:
- Enforce business rules
- Validate before state changes
- Separate intent (commands) from facts (events)

### Q: Why is the event store in-memory?
**A:** For simplicity in this learning example. Real applications use persistent stores like SQLite or PostgreSQL.

### Q: What happens to events when the program exits?
**A:** They're lost (in-memory only). Use `eventium-sqlite` or `eventium-postgresql` for persistence.

## License

MIT - see [LICENSE.md](../../LICENSE.md)

