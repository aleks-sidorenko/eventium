# Design: Remove Record Field Prefixes

## Goal

Adopt the "Do not prefix record fields" convention from [Freckle's Haskell best practices](https://github.com/freckle/guides/blob/main/haskell-best-practices.md#do-not-prefix-record-fields), using modern GHC extensions to eliminate type-name prefixes from record fields across the entire project.

## Decisions

- **Scope:** All packages (core, memory, sql-common, postgresql, sqlite, testkit, examples)
- **Lens fields in examples:** Replace lenses with `OverloadedRecordDot` (remove `makeLenses`, lens dependency)
- **RecordWildCards:** Replace with explicit field patterns or dot syntax
- **Newtype `un*` unwrappers:** Keep unchanged (idiomatic, different purpose)
- **Migration strategy:** Package-by-package, one commit per package

## Extensions

Enable as default extensions in all `package.yaml` files:

- `NoFieldSelectors` -- prevents field names from generating top-level accessor functions
- `DuplicateRecordFields` -- allows multiple types to share field names
- `OverloadedRecordDot` -- enables `record.field` access syntax

Remove `RecordWildCards` from all modules.

## Field Renaming Rules

1. Strip the type-name prefix: `projectionSeed` -> `seed`, `streamEventKey` -> `key`
2. Keep `un*` unwrappers unchanged: `unEventVersion`, `unSequenceNumber`, `unRejectionReason`
3. Already-clean types stay unchanged: `Codec`, `TypeEmbedding`, `EventHandler`, `EventPublisher`, `CheckpointStore`, `CommandDispatcher`
4. Lens-prefixed fields: drop `_typeName` prefix, remove `makeLenses`

## Call Site Migration Patterns

| Before | After |
|--------|-------|
| `projectionSeed p` | `p.seed` |
| `let Projection{..} = p in projectionSeed` | `p.seed` or `Projection{seed, eventHandler}` |
| `p { streamProjectionPosition = pos }` | `p { position = pos }` |
| `view accountBalance acct` | `acct.balance` |

## Types Affected

### eventium-core

| Type | Old Fields | New Fields |
|------|-----------|------------|
| `Projection` | `projectionSeed`, `projectionEventHandler` | `seed`, `eventHandler` |
| `StreamProjection` | `streamProjectionKey`, `streamProjectionPosition`, `streamProjectionProjection`, `streamProjectionState` | `key`, `position`, `projection`, `state` |
| `EventMetadata` | `eventMetadataEventType`, `eventMetadataCorrelationId`, `eventMetadataCausationId`, `eventMetadataCreatedAt` | `eventType`, `correlationId`, `causationId`, `createdAt` |
| `TaggedEvent` | `taggedEventMetadata`, `taggedEventPayload` | `metadata`, `payload` |
| `StreamEvent` | `streamEventKey`, `streamEventPosition`, `streamEventMetadata`, `streamEventPayload` | `key`, `position`, `metadata`, `payload` |
| `QueryRange` | `queryRangeKey`, `queryRangeStart`, `queryRangeLimit` | `key`, `start`, `limit` |
| `RetryConfig` | `retryInitialDelayMs`, `retryMaxDelayMs`, `retryBackoffMultiplier`, `retryOnError`, `retryOnErrorCallback` | `initialDelayMs`, `maxDelayMs`, `backoffMultiplier`, `onError`, `onErrorCallback` |
| `CommandHandler` | `commandHandlerDecide`, `commandHandlerProjection` | `decide`, `projection` |
| `ProcessManager` | `processManagerProjection`, `processManagerReact` | `projection`, `react` |
| `DecodeError` | `decodeErrorContext`, `decodeErrorMessage` | `context`, `message` |
| `EncodeError` | `encodeErrorContext`, `encodeErrorMessage` | `context`, `message` |

### eventium-sql-common

| Type | Old Fields | New Fields |
|------|-----------|------------|
| `SqlEventStoreConfig` | `sqlEventStoreConfigSequenceNumberField`, ... (all prefixed with `sqlEventStoreConfig`) | `sequenceNumberField`, ... |

### examples/bank

| Type | Old Fields | New Fields |
|------|-----------|------------|
| `Account` | `_accountBalance`, `_accountOwner`, `_accountPendingTransfers` | `balance`, `owner`, `pendingTransfers` |
| `PendingAccountTransfer` | `pendingAccountTransferId`, `pendingAccountTransferAmount`, `pendingAccountTransferTargetAccount` | `id`, `amount`, `targetAccount` |
| `CustomerAccounts` | `_customerAccountsAccountsById`, `_customerAccountsCustomerAccounts`, `_customerAccountsCustomerIdsByName` | `accountsById`, `customerAccounts`, `customerIdsByName` |

## Migration Order

1. **eventium-core** -- Enable extensions, rename all core type fields, update internal call sites, remove RecordWildCards
2. **eventium-memory** -- Enable extensions, update call sites for core types, remove RecordWildCards
3. **eventium-sql-common** -- Enable extensions, rename SqlEventStoreConfig fields, update internal call sites
4. **eventium-postgresql** & **eventium-sqlite** -- Enable extensions, update call sites
5. **eventium-testkit** -- Enable extensions, migrate heavy field accessor usage to dot syntax
6. **examples** -- Enable extensions, remove lens dependency, rename fields, replace lens operations with dot syntax

## Testing

After each package: `cabal test <package>`. After core: run all downstream tests too. Full `just test` after each step.

## Unchanged

- `Codec` (`encode`/`decode`), `TypeEmbedding` (`embed`/`extract`), `EventHandler`, `EventPublisher`, `CheckpointStore`, `CommandDispatcher`
- `un*` newtype unwrappers
- Template Haskell in `Eventium.TH` (generates sum-type codecs, not record fields)
