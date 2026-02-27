# Simplify Command Error Plumbing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Introduce `RejectionReason` newtype, simplify `AggregateHandler` construction, and replace raw `Text` at dispatch/PM boundary.

**Architecture:** Add `RejectionReason` newtype to `Eventium.ProcessManager`, update `CommandDispatchResult` and `ProcessManagerEffect` to use it, change `AggregateHandler` formatter from `err -> Text` to `err -> RejectionReason` with a `Show`-based default constructor.

**Tech Stack:** Haskell, cabal, hspec. Build with `just build`, test with `cabal test eventium-core && cabal test eventium-memory && cabal test examples-bank`.

**Design doc:** `docs/plans/2026-02-24-simplify-command-error-plumbing-design.md`

---

### Task 1: Add `RejectionReason` newtype to ProcessManager module

**Files:**
- Modify: `eventium-core/src/Eventium/ProcessManager.hs:1-24` (imports and exports)
- Modify: `eventium-core/src/Eventium/ProcessManager.hs:48-74` (types)

**Step 1: Add `RejectionReason` to module exports**

In `eventium-core/src/Eventium/ProcessManager.hs`, add `RejectionReason(..)` to the export list (line 17):

```haskell
module Eventium.ProcessManager
  ( ProcessManager (..),
    ProcessManagerEffect (..),
    CommandDispatchResult (..),
    RejectionReason (..),
    CommandDispatcher (..),
    mkCommandDispatcher,
    fireAndForgetDispatcher,
    runProcessManagerEffects,
    processManagerEventHandler,
  )
where
```

**Step 2: Add `OverloadedStrings` and `IsString` import**

Add at the top of the file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

Add to imports:

```haskell
import Data.String (IsString)
```

**Step 3: Define `RejectionReason` newtype**

Add before the `ProcessManagerEffect` definition (around line 46):

```haskell
-- | A typed wrapper for the reason a command was rejected.
--
-- Use 'RejectionReason' instead of raw 'Text' to avoid accidentally
-- mixing rejection reasons with other textual values at the dispatch boundary.
newtype RejectionReason = RejectionReason {unRejectionReason :: Text}
  deriving (Show, Eq, Ord, IsString)
```

**Step 4: Update `CommandDispatchResult` to use `RejectionReason`**

Change the `CommandFailed` constructor (line 73):

```haskell
data CommandDispatchResult
  = -- | The command was accepted and events were stored.
    CommandSucceeded
  | -- | The command was rejected by the aggregate with a reason.
    CommandFailed RejectionReason
  deriving (Show, Eq)
```

**Step 5: Update `ProcessManagerEffect` to use `RejectionReason`**

Change the `IssueCommandWithCompensation` constructor (line 56):

```haskell
  | IssueCommandWithCompensation UUID command (RejectionReason -> [ProcessManagerEffect command])
```

**Step 6: Build to check compilation**

Run: `just build`
Expected: Should compile (no downstream consumers updated yet will cause failures in tests/examples, but the core module itself should compile).

**Step 7: Commit**

```bash
git add eventium-core/src/Eventium/ProcessManager.hs
git commit -m "feat(eventium-core): add RejectionReason newtype, update CommandDispatchResult and ProcessManagerEffect"
```

---

### Task 2: Update `CommandDispatcher` module

**Files:**
- Modify: `eventium-core/src/Eventium/CommandDispatcher.hs:1-65`

**Step 1: Add `OverloadedStrings` pragma**

Add at top of file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

**Step 2: Update import of `ProcessManager`**

Add `RejectionReason(..)` to the import (line 18):

```haskell
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher, RejectionReason (..), mkCommandDispatcher)
```

**Step 3: Update `AggregateHandler` type**

Change the formatter field from `(err -> Text)` to `(err -> RejectionReason)` (line 29):

```haskell
data AggregateHandler event command
  = forall state err.
    AggregateHandler
      (CommandHandler state event command err)
      (err -> RejectionReason)
```

**Step 4: Add `Show`-based `mkAggregateHandler` and `mkAggregateHandlerWith`**

Update the export list to include `mkAggregateHandlerWith`:

```haskell
module Eventium.CommandDispatcher
  ( AggregateHandler,
    mkAggregateHandler,
    mkAggregateHandlerWith,
    commandHandlerDispatcher,
  )
where
```

Replace the current `mkAggregateHandler` (lines 33-37) with:

```haskell
-- | Construct an 'AggregateHandler' using 'Show' to format errors.
mkAggregateHandler ::
  (Show err) =>
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandler h = AggregateHandler h (RejectionReason . T.pack . show)

-- | Construct an 'AggregateHandler' with an explicit error formatter.
mkAggregateHandlerWith ::
  (err -> RejectionReason) ->
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandlerWith fmt h = AggregateHandler h fmt
```

**Step 5: Update `commandHandlerDispatcher` concurrency conflict case**

Change line 64 from `T.pack "Concurrency conflict"` to use `RejectionReason`:

```haskell
        Left (ConcurrencyConflict _) -> pure (CommandFailed "Concurrency conflict")
```

(`OverloadedStrings` + `IsString` on `RejectionReason` handles the conversion.)

**Step 6: Remove unused `Data.Text` import if applicable**

The `T.pack` usage remains in `mkAggregateHandler`, so keep the import. But verify no dead imports remain.

**Step 7: Build to check compilation**

Run: `just build`
Expected: Core compiles. Tests and examples may fail until updated.

**Step 8: Commit**

```bash
git add eventium-core/src/Eventium/CommandDispatcher.hs
git commit -m "feat(eventium-core): update AggregateHandler to use RejectionReason, add Show-based mkAggregateHandler"
```

---

### Task 3: Update `CommandDispatcherSpec` tests

**Files:**
- Modify: `eventium-core/tests/Eventium/CommandDispatcherSpec.hs`

**Step 1: Update import to include `RejectionReason`**

Change line 9:

```haskell
import Eventium.ProcessManager (CommandDispatchResult (..), RejectionReason (..), dispatchCommand)
```

**Step 2: Remove `formatCounterError` function**

Delete lines 39-40 (`formatCounterError` is no longer needed since `mkAggregateHandler` uses `Show`):

```haskell
-- DELETE:
-- formatCounterError :: CounterError -> Text
-- formatCounterError AlreadyZero = "Already at zero"
```

**Step 3: Update handler construction to use `mkAggregateHandler` without formatter**

Change lines 68, 77, 87 from:

```haskell
let handlers = [mkAggregateHandler counterHandler formatCounterError]
```

to:

```haskell
let handlers = [mkAggregateHandler counterHandler]
```

**Step 4: Update expected `CommandFailed` value**

Line 82 changes from:

```haskell
result `shouldBe` CommandFailed "Already at zero"
```

to (since `Show` on `AlreadyZero` produces `"AlreadyZero"`):

```haskell
result `shouldBe` CommandFailed (RejectionReason "AlreadyZero")
```

**Step 5: Remove unused `Data.Text` import**

Line 6 `import Data.Text (Text)` is no longer needed. Remove it.

**Step 6: Run tests**

Run: `cabal test eventium-core`
Expected: All 3 CommandDispatcher tests pass.

**Step 7: Commit**

```bash
git add eventium-core/tests/Eventium/CommandDispatcherSpec.hs
git commit -m "test(eventium-core): update CommandDispatcherSpec for RejectionReason"
```

---

### Task 4: Update `ProcessManagerSpec` tests

**Files:**
- Modify: `eventium-memory/tests/Eventium/ProcessManagerSpec.hs`

**Step 1: Add `OverloadedStrings` pragma**

The file already has `{-# LANGUAGE OverloadedStrings #-}` at line 1. Verify it's present.

**Step 2: Update `CommandFailed` usage**

Line 131 changes from:

```haskell
then pure (CommandFailed "rejected")
```

to:

```haskell
then pure (CommandFailed "rejected")
```

This actually stays the same because `OverloadedStrings` + `IsString RejectionReason` means `"rejected"` is automatically a `RejectionReason`. No change needed here.

**Step 3: Run tests**

Run: `cabal test eventium-memory`
Expected: All ProcessManager tests pass.

**Step 4: Commit (if any changes were needed)**

```bash
git add eventium-memory/tests/Eventium/ProcessManagerSpec.hs
git commit -m "test(eventium-memory): update ProcessManagerSpec for RejectionReason"
```

---

### Task 5: Update Bank example — Store.hs

**Files:**
- Modify: `examples/bank/src/Bank/CLI/Store.hs:33-46`

**Step 1: Update `mkAggregateHandler` call**

Change line 37 from:

```haskell
[mkAggregateHandler accountBankCommandHandler formatAccountError]
```

to either use `Show`-based default:

```haskell
[mkAggregateHandler accountBankCommandHandler]
```

or to keep the custom formatter with `mkAggregateHandlerWith`:

```haskell
[mkAggregateHandlerWith (RejectionReason . formatAccountError) accountBankCommandHandler]
```

The custom formatter produces nicer messages like "Insufficient funds (balance: 42.0)" vs the `Show` default "InsufficientFunds 42.0". Use `mkAggregateHandlerWith` to preserve the existing behavior.

**Step 2: Update `formatAccountError` return type**

Change lines 42-46. The function currently returns `Text`. Either:

a) Keep `formatAccountError :: AccountCommandError -> Text` and wrap at call site (done in Step 1), or
b) Change it to return `RejectionReason`:

```haskell
formatAccountError :: AccountCommandError -> RejectionReason
formatAccountError AccountAlreadyOpen = "Account already open"
formatAccountError InvalidInitialDeposit = "Invalid initial deposit"
formatAccountError (InsufficientFunds balance) = RejectionReason . pack $ "Insufficient funds (balance: " ++ show balance ++ ")"
formatAccountError AccountNotOpen = "Account not open"
```

Option (b) is cleaner. Add `{-# LANGUAGE OverloadedStrings #-}` to the file and import `RejectionReason` from `Eventium`.

**Step 3: Update `mkAggregateHandler` call to use `mkAggregateHandlerWith`**

```haskell
[mkAggregateHandlerWith formatAccountError accountBankCommandHandler]
```

**Step 4: Build bank example**

Run: `cabal build examples-bank`
Expected: Compiles.

**Step 5: Commit**

```bash
git add examples/bank/src/Bank/CLI/Store.hs
git commit -m "refactor(examples-bank): use RejectionReason in Store.hs"
```

---

### Task 6: Update Bank example — TransferManager.hs

**Files:**
- Modify: `examples/bank/src/Bank/ProcessManagers/TransferManager.hs:54-77`

**Step 1: Update compensation callback**

The callback on line 66 currently takes `Text`. With `RejectionReason`, update the pattern:

Change:

```haskell
          ( \reason ->
              [ IssueCommand
                  sourceAccount
                  ( RejectTransferCommand
                      RejectTransfer
                        { rejectTransferTransferId = accountTransferStartedTransferId,
                          rejectTransferReason = T.unpack reason
                        }
                  )
              ]
          )
```

to:

```haskell
          ( \(RejectionReason reason) ->
              [ IssueCommand
                  sourceAccount
                  ( RejectTransferCommand
                      RejectTransfer
                        { rejectTransferTransferId = accountTransferStartedTransferId,
                          rejectTransferReason = T.unpack reason
                        }
                  )
              ]
          )
```

This imports `RejectionReason(..)` from `Eventium` (already re-exported).

**Step 2: Build bank example**

Run: `cabal build examples-bank`
Expected: Compiles.

**Step 3: Run bank tests**

Run: `cabal test examples-bank`
Expected: All tests pass.

**Step 4: Commit**

```bash
git add examples/bank/src/Bank/ProcessManagers/TransferManager.hs
git commit -m "refactor(examples-bank): update TransferManager compensation for RejectionReason"
```

---

### Task 7: Full test suite and cleanup

**Files:**
- All previously modified files

**Step 1: Run full build**

Run: `just build`
Expected: All packages compile.

**Step 2: Run full test suite**

Run: `cabal test eventium-core && cabal test eventium-memory && cabal test examples-bank`
Expected: All tests pass.

**Step 3: Run hlint**

Run: `hlint eventium-core/src/Eventium/ProcessManager.hs eventium-core/src/Eventium/CommandDispatcher.hs`
Expected: No warnings (or only pre-existing ones).

**Step 4: Run ormolu**

Run: `just format`
Expected: No formatting changes, or apply them if needed.

**Step 5: Final commit if any formatting/lint changes**

```bash
git add -A
git commit -m "chore: apply formatting"
```
