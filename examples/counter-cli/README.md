# Counter CLI Example

Minimal event sourcing example: a bounded counter with an in-memory store.

## Overview

A single-file interactive REPL demonstrating the core event sourcing concepts:
events, commands, projections, and a command handler -- all in ~100 lines.

## Running

```bash
cabal run counter-cli
```

### Session

```
Current state: CounterState {unCounterState = 0}
Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):
IncrementCounter 10
Events generated: [CounterAmountAdded 10]

Current state: CounterState {unCounterState = 10}
```

The counter is bounded to 0--100. Out-of-range operations emit
`CounterOutOfBounds` without changing state.

## Concepts Demonstrated

| Concept | Implementation |
|---------|---------------|
| **Events** | `CounterAmountAdded Int`, `CounterOutOfBounds Int` |
| **Commands** | `IncrementCounter`, `DecrementCounter`, `ResetCounter` |
| **Projection** | Pure fold from events to `CounterState` |
| **CommandHandler** | Validates bounds, returns `Either Void [CounterEvent]` |
| **In-memory store** | `tvarEventStoreWriter` / `tvarEventStoreReader` from `eventium-memory` |

The command handler uses `Void` as the error type since all commands produce
events (out-of-bounds is modeled as an event, not a rejection).

## Code Structure

Everything is in `counter-cli.hs`:

```haskell
-- State
newtype CounterState = CounterState { unCounterState :: Int }

-- Command handler (4 type params: state, event, command, err)
type CounterCommandHandler = CommandHandler CounterState CounterEvent CounterCommand Void

-- Decision function
handlerCounterCommand :: CounterState -> CounterCommand -> Either Void [CounterEvent]
```

## Next Steps

- [Cafe Example](../cafe/) -- workflow patterns, SQLite persistence, polling subscription
- [Bank Example](../bank/) -- multi-aggregate, process manager, event publisher
- [Design](../../DESIGN.md)

## License

MIT -- see [LICENSE.md](../../LICENSE.md)
