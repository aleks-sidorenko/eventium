# Simplify Command Error Plumbing and PM Compensation

## Context

The eventium library's command handling pipeline involves several types that plumb
domain errors from aggregates through dispatchers to process managers:

- `CommandHandler state event command err` -- 4 type params, `err` for domain rejections
- `CommandHandlerError err` -- wraps `CommandRejected err | ConcurrencyConflict`
- `AggregateHandler event command` -- existentially erases `state` and `err`, requires `err -> Text`
- `CommandDispatchResult` -- `CommandSucceeded | CommandFailed Text`
- `ProcessManagerEffect command` -- `IssueCommandWithCompensation` takes `Text -> [Effect]`

The raw `Text` at the dispatch/PM boundary loses type safety and makes it easy to
confuse rejection reasons with other text values.

## Design Decision: Keep `Either err` in CommandHandler

We considered three approaches:

1. **Remove `Either err` entirely** -- all outcomes become events (`commandHandlerDecide :: state -> command -> [event]`)
2. **Parallel types** -- add `TotalCommandHandler` alongside existing
3. **Clean break with events** -- failure events stored in event store

After reviewing DDD/event sourcing literature (Greg Young, Jonathan Oliver, community
consensus), we chose to **keep `Either err`**. The mainstream recommendation is:

- Events represent facts that happened (state changes the domain accepted)
- Rejected commands are not events -- they don't change state
- Saga failure tracking belongs in saga infrastructure, not aggregate event streams
- Storing rejection events pollutes the event log with "things that didn't happen"

The current architecture already follows this pattern correctly.

## Changes

### 1. Introduce `RejectionReason` newtype

Replace raw `Text` at the dispatch/PM boundary with a semantic newtype:

```haskell
newtype RejectionReason = RejectionReason { unRejectionReason :: Text }
  deriving (Show, Eq, Ord, IsString)
```

This prevents accidentally mixing rejection text with other `Text` values.

### 2. Update `CommandDispatchResult`

```haskell
data CommandDispatchResult
  = CommandSucceeded
  | CommandFailed RejectionReason  -- was: CommandFailed Text
```

### 3. Update `ProcessManagerEffect`

```haskell
data ProcessManagerEffect command
  = IssueCommand UUID command
  | IssueCommandWithCompensation UUID command (RejectionReason -> [ProcessManagerEffect command])
  -- was: (Text -> [ProcessManagerEffect command])
```

### 4. Update `AggregateHandler` with `Show`-based smart constructor

```haskell
data AggregateHandler event command
  = forall state err.
    AggregateHandler
      (CommandHandler state event command err)
      (err -> RejectionReason)  -- was: (err -> Text)

-- Smart constructor: defaults to Show for formatting
mkAggregateHandler :: (Show err) => CommandHandler s e c err -> AggregateHandler e c
mkAggregateHandler h = AggregateHandler h (RejectionReason . T.pack . show)

-- Explicit formatter version
mkAggregateHandlerWith :: (err -> RejectionReason) -> CommandHandler s e c err -> AggregateHandler e c
mkAggregateHandlerWith fmt h = AggregateHandler h fmt
```

### 5. Update `commandHandlerDispatcher`

Use `RejectionReason` in the concurrency conflict case:

```haskell
Left (ConcurrencyConflict _) -> pure (CommandFailed (RejectionReason "Concurrency conflict"))
```

## What Does NOT Change

- `CommandHandler state event command err` -- keeps all 4 type params
- `CommandHandlerError err` -- keeps `CommandRejected err | ConcurrencyConflict`
- `applyCommandHandler` -- return type unchanged
- `codecCommandHandler` / `embeddedCommandHandler` -- unchanged
- Domain error types (e.g., `AccountCommandError`) -- unchanged
- `ProcessManager` type -- unchanged
- Process manager `react` function signature -- unchanged

## Impact on Examples

**Bank TransferManager:** The compensation callback changes from `\reason -> ...` to
`\(RejectionReason reason) -> ...` or can use `unRejectionReason` to extract the text.
Minimal change.

**Bank CLI:** No change -- uses `applyCommandHandler` directly, doesn't go through
dispatcher.

## What This Achieves

1. **Type safety at the boundary** -- `RejectionReason` prevents mixing with other `Text`
2. **Less boilerplate** -- `mkAggregateHandler` needs only `Show` constraint, no explicit formatter
3. **Better ergonomics** -- `IsString` lets you write string literals directly
4. **Backward compatible spirit** -- all existing patterns work with minor type adjustments
5. **Mainstream DDD/ES alignment** -- `Either err` preserved, rejections stay ephemeral
