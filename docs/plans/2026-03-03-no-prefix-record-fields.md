# Remove Record Field Prefixes — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove type-name prefixes from record fields across the entire project, using `NoFieldSelectors`, `DuplicateRecordFields`, and `OverloadedRecordDot`.

**Architecture:** Package-by-package migration (core → memory → sql-common → backends → testkit → examples). Each package: enable extensions in `package.yaml`, rename fields, update all call sites to use dot syntax or explicit patterns, remove `RecordWildCards`, test, commit.

**Tech Stack:** GHC 9.6.7, `OverloadedRecordDot`, `NoFieldSelectors`, `DuplicateRecordFields`, cabal, hpack

**Key gotcha:** `EventMetadata` has `genericToJSON (unPrefixLower "eventMetadata")` — after renaming fields, change to `defaultOptions` to preserve the same JSON keys. Similarly, testkit's `deriveJSON (aesonPrefix camelCase)` must be updated to `deriveJSON defaultOptions` after renaming.

---

### Task 1: eventium-core — Enable extensions and update package.yaml

**Files:**
- Modify: `eventium-core/package.yaml`

**Step 1: Add default-extensions to package.yaml**

Add a `default-extensions` block under `library:` and `tests:`:

```yaml
library:
  source-dirs:
    - src
  ghc-options: -Wall
  default-extensions:
    - NoFieldSelectors
    - DuplicateRecordFields
    - OverloadedRecordDot
```

The tests section should also get these extensions since it includes `src` in source-dirs:

```yaml
tests:
  spec:
    main: Spec.hs
    ghc-options: -Wall
    source-dirs:
      - tests
      - src
    dependencies:
      - hspec
      - HUnit
    build-tools:
      - hspec-discover
    default-extensions:
      - NoFieldSelectors
      - DuplicateRecordFields
      - OverloadedRecordDot
```

**Step 2: Regenerate cabal file**

Run: `just hpack`

---

### Task 2: eventium-core — Rename fields in Projection.hs

**Files:**
- Modify: `eventium-core/src/Eventium/Projection.hs`

**Step 1: Rename Projection fields (lines 37-44)**

Before:
```haskell
data Projection state event
  = Projection
  { projectionSeed :: state,
    projectionEventHandler :: state -> event -> state
  }
```

After:
```haskell
data Projection state event
  = Projection
  { seed :: state,
    eventHandler :: state -> event -> state
  }
```

**Step 2: Rename StreamProjection fields (lines 66-72)**

Before:
```haskell
data StreamProjection key position state event
  = StreamProjection
  { streamProjectionKey :: !key,
    streamProjectionPosition :: !position,
    streamProjectionProjection :: !(Projection state event),
    streamProjectionState :: !state
  }
```

After:
```haskell
data StreamProjection key position state event
  = StreamProjection
  { key :: !key,
    position :: !position,
    projection :: !(Projection state event),
    state :: !state
  }
```

**Step 3: Remove `RecordWildCards` pragma (line 1) and update all call sites**

Remove `{-# LANGUAGE RecordWildCards #-}` from line 1.

Update `Contravariant` instance (line 47):
```haskell
instance Contravariant (Projection state) where
  contramap f proj = Projection proj.seed handler'
    where
      handler' st event = proj.eventHandler st (f event)
```

Update `latestProjection` (line 53):
```haskell
latestProjection proj = foldl' proj.eventHandler proj.seed
```

Update `allProjections` (line 60):
```haskell
allProjections proj = scanl' proj.eventHandler proj.seed
```

Update `streamProjection` (lines 84-85):
```haskell
streamProjection k pos proj =
  StreamProjection
    { key = k,
      position = pos,
      projection = proj,
      state = proj.seed
    }
```

Update `streamProjectionEventHandler` (lines 108-112):
```haskell
streamProjectionEventHandler sp streamEvent =
  let pos' = streamEvent.position
      state' = sp.projection.eventHandler sp.state streamEvent.payload
   in StreamProjection sp.key pos' sp.projection state'
```

Note: This function name `streamProjectionEventHandler` is exported and used externally. The function name stays the same (it's not a record field, it's a standalone function). Consider renaming it later if desired, but for this task keep it.

Update `getLatestStreamProjection` (lines 121-123):
```haskell
getLatestStreamProjection (EventStoreReader getEvents') sp = do
  events <- getEvents' (eventsStartingAt sp.key $ sp.position + 1)
  return $ foldl' streamProjectionEventHandler sp events
```

Update `codecProjection` (lines 132-138):
```haskell
codecProjection codec proj =
  Projection proj.seed handler'
  where
    handler' st encoded =
      case decode codec encoded of
        Just event -> proj.eventHandler st event
        Nothing -> throw $ DecodeError "codecProjection" "Failed to decode event"
```

Update `projectionMapMaybe` (lines 168-170):
```haskell
projectionMapMaybe f proj = Projection proj.seed handler'
  where
    handler' st = maybe st (proj.eventHandler st) . f
```

**Step 4: Update module exports (lines 3-19)**

Remove `streamProjectionEventHandler` from exports if it was only there as an accessor — but it's actually a standalone function (not a field accessor), so keep it. The record constructor exports `Projection (..)` and `StreamProjection (..)` already export all fields.

---

### Task 3: eventium-core — Rename fields in Store/Types.hs

**Files:**
- Modify: `eventium-core/src/Eventium/Store/Types.hs`

**Step 1: Rename EventMetadata fields (lines 40-45)**

Before:
```haskell
data EventMetadata = EventMetadata
  { eventMetadataEventType :: !Text,
    eventMetadataCorrelationId :: !(Maybe UUID),
    eventMetadataCausationId :: !(Maybe UUID),
    eventMetadataCreatedAt :: !(Maybe UTCTime)
  }
```

After:
```haskell
data EventMetadata = EventMetadata
  { eventType :: !Text,
    correlationId :: !(Maybe UUID),
    causationId :: !(Maybe UUID),
    createdAt :: !(Maybe UTCTime)
  }
```

**Step 2: Update JSON instances (lines 48-53)**

Before:
```haskell
instance ToJSON EventMetadata where
  toJSON = genericToJSON (unPrefixLower "eventMetadata")
  toEncoding = genericToEncoding (unPrefixLower "eventMetadata")

instance FromJSON EventMetadata where
  parseJSON = genericParseJSON (unPrefixLower "eventMetadata")
```

After:
```haskell
instance ToJSON EventMetadata where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance FromJSON EventMetadata where
  parseJSON = genericParseJSON defaultOptions
```

The import of `Eventium.Json (unPrefixLower)` can be removed from this file (line 33). Add `Data.Aeson (defaultOptions)` if not already imported — it's already imported via `Data.Aeson`.

**Step 3: Update `emptyMetadata` (line 57)**

This uses positional construction. Keep positional since it's clear with 4 args:
```haskell
emptyMetadata et = EventMetadata et Nothing Nothing Nothing
```
No change needed — positional construction still works with `NoFieldSelectors`.

**Step 4: Rename TaggedEvent fields (lines 63-66)**

Before:
```haskell
data TaggedEvent event = TaggedEvent
  { taggedEventMetadata :: !EventMetadata,
    taggedEventPayload :: !event
  }
```

After:
```haskell
data TaggedEvent event = TaggedEvent
  { metadata :: !EventMetadata,
    payload :: !event
  }
```

**Step 5: Rename StreamEvent fields (lines 71-77)**

Before:
```haskell
data StreamEvent key position event
  = StreamEvent
  { streamEventKey :: !key,
    streamEventPosition :: !position,
    streamEventMetadata :: !EventMetadata,
    streamEventPayload :: !event
  }
```

After:
```haskell
data StreamEvent key position event
  = StreamEvent
  { key :: !key,
    position :: !position,
    metadata :: !EventMetadata,
    payload :: !event
  }
```

**Step 6: Remove unused import**

Remove `Eventium.Json (unPrefixLower)` import from this file since it's no longer used after the JSON instance change.

---

### Task 4: eventium-core — Rename fields in Store/Queries.hs

**Files:**
- Modify: `eventium-core/src/Eventium/Store/Queries.hs`

**Step 1: Rename QueryRange fields (lines 18-23)**

Before:
```haskell
data QueryRange key position
  = QueryRange
  { queryRangeKey :: key,
    queryRangeStart :: QueryStart position,
    queryRangeLimit :: QueryLimit position
  }
```

After:
```haskell
data QueryRange key position
  = QueryRange
  { key :: key,
    start :: QueryStart position,
    limit :: QueryLimit position
  }
```

**Step 2: Update helper functions (lines 39-52)**

These use positional construction (`QueryRange key StartFromBeginning NoQueryLimit`). Positional construction still works with `NoFieldSelectors`. No change needed for these functions.

---

### Task 5: eventium-core — Rename fields in Codec.hs

**Files:**
- Modify: `eventium-core/src/Eventium/Codec.hs`

**Step 1: Rename DecodeError fields (lines 56-59)**

Before:
```haskell
data DecodeError = DecodeError
  { decodeErrorContext :: !String,
    decodeErrorMessage :: !String
  }
```

After:
```haskell
data DecodeError = DecodeError
  { context :: !String,
    message :: !String
  }
```

**Step 2: Rename EncodeError fields (lines 67-70)**

Before:
```haskell
data EncodeError = EncodeError
  { encodeErrorContext :: !String,
    encodeErrorMessage :: !String
  }
```

After:
```haskell
data EncodeError = EncodeError
  { context :: !String,
    message :: !String
  }
```

**Step 3: Remove `RecordWildCards` pragma and update `traverseCodec` (line 94)**

Remove `{-# LANGUAGE RecordWildCards #-}` from line 4.

Before:
```haskell
traverseCodec Codec {..} =
  Codec encode' decode'
  where
    encode' = fmap encode
    decode' = traverse decode
```

After:
```haskell
traverseCodec codec =
  Codec encode' decode'
  where
    encode' = fmap (encode codec)
    decode' = traverse (decode codec)
```

Note: `Codec` fields `encode` and `decode` are already unprefixed. They don't need renaming. But with `NoFieldSelectors`, they can no longer be used as top-level functions. The existing code in `composeCodecs` (line 81) uses `encode codec2 . encode codec1` — this calls the `encode` function from the `Codec` module. With `NoFieldSelectors`, `encode` is no longer a top-level function. We need to switch to dot syntax.

Update `composeCodecs` (lines 79-82):
```haskell
composeCodecs codec1 codec2 = Codec encode' decode'
  where
    encode' = codec2.encode . codec1.encode
    decode' x = codec2.decode x >>= codec1.decode
```

Update `eventSumTypeCodec` (lines 128-134) — `encode` and `decode` are not used as Codec field accessors here. They don't need changing since they refer to the `EventSumType` class methods, not `Codec` fields.

Check `codecProjection` in `Projection.hs` — it uses `decode codec encoded` which would need to become `codec.decode encoded`. This was already handled in Task 2.

**Step 4: Update all Codec field usages across eventium-core**

Every place that uses `encode someCodec` or `decode someCodec` as a function call must be changed to `someCodec.encode` or `someCodec.decode`. Check:
- `Projection.hs:136`: `decode codec encoded` → `codec.decode encoded`
- `Projection.hs:150`: `decode codec` → `codec.decode`
- `Projection.hs:160`: `extract emb` → `emb.extract`
- `CommandHandler.hs:88-90`: `decode commandCodec`, `encode eventCodec` → dot syntax
- `CommandHandler.hs:108-112`: `extract commandEmb`, `embed eventEmb` → dot syntax
- `TypeEmbedding.hs:38-39`: `embed emb2 . embed emb1`, `extract emb2 >=> extract emb1`

For `TypeEmbedding.hs`, update `composeTypeEmbeddings` or similar:
```haskell
-- Before
{ embed = embed emb2 . embed emb1,
  extract = extract emb2 >=> extract emb1
}
-- After
{ embed = emb2.embed . emb1.embed,
  extract = emb2.extract >=> emb1.extract
}
```

---

### Task 6: eventium-core — Rename fields in CommandHandler.hs

**Files:**
- Modify: `eventium-core/src/Eventium/CommandHandler.hs`

**Step 1: Rename CommandHandler fields (lines 34-38)**

Before:
```haskell
data CommandHandler state event command err
  = CommandHandler
  { commandHandlerDecide :: state -> command -> Either err [event],
    commandHandlerProjection :: Projection state event
  }
```

After:
```haskell
data CommandHandler state event command err
  = CommandHandler
  { decide :: state -> command -> Either err [event],
    projection :: Projection state event
  }
```

**Step 2: Remove `RecordWildCards` pragma and update `applyCommandHandler` (lines 63-71)**

Remove `{-# LANGUAGE RecordWildCards #-}` from line 1.

Before:
```haskell
applyCommandHandler writer reader (CommandHandler handler proj) uuid command = do
  StreamProjection {..} <- getLatestStreamProjection reader (versionedStreamProjection uuid proj)
  case handler streamProjectionState command of
    Left err -> return $ Left (CommandRejected err)
    Right events -> do
      result <- storeEvents writer uuid (ExactPosition streamProjectionPosition) events
```

After:
```haskell
applyCommandHandler writer reader cmdHandler uuid command = do
  sp <- getLatestStreamProjection reader (versionedStreamProjection uuid cmdHandler.projection)
  case cmdHandler.decide sp.state command of
    Left err -> return $ Left (CommandRejected err)
    Right events -> do
      result <- storeEvents writer uuid (ExactPosition sp.position) events
```

**Step 3: Update `codecCommandHandler` (lines 83-90)**

Before:
```haskell
codecCommandHandler eventCodec commandCodec (CommandHandler commandHandler projection) =
  CommandHandler codecHandler codecProjection'
  where
    codecProjection' = codecProjection eventCodec projection
    codecHandler state encodedCmd =
      case decode commandCodec encodedCmd of
        Nothing -> throw $ DecodeError "codecCommandHandler" "Failed to decode command"
        Just cmd -> fmap (map (encode eventCodec)) (commandHandler state cmd)
```

After:
```haskell
codecCommandHandler eventCodec commandCodec cmdHandler =
  CommandHandler codecHandler codecProjection'
  where
    codecProjection' = codecProjection eventCodec cmdHandler.projection
    codecHandler st encodedCmd =
      case commandCodec.decode encodedCmd of
        Nothing -> throw $ DecodeError "codecCommandHandler" "Failed to decode command"
        Just cmd -> fmap (map eventCodec.encode) (cmdHandler.decide st cmd)
```

**Step 4: Update `embeddedCommandHandler` (lines 105-112)**

Before:
```haskell
embeddedCommandHandler eventEmb commandEmb (CommandHandler commandHandler projection) =
  CommandHandler embeddedHandler embeddedProjection'
  where
    embeddedProjection' = embeddedProjection eventEmb projection
    embeddedHandler state adaptedCmd =
      case extract commandEmb adaptedCmd of
        Nothing -> Right []
        Just cmd -> fmap (map (embed eventEmb)) (commandHandler state cmd)
```

After:
```haskell
embeddedCommandHandler eventEmb commandEmb cmdHandler =
  CommandHandler embeddedHandler embeddedProjection'
  where
    embeddedProjection' = embeddedProjection eventEmb cmdHandler.projection
    embeddedHandler st adaptedCmd =
      case commandEmb.extract adaptedCmd of
        Nothing -> Right []
        Just cmd -> fmap (map eventEmb.embed) (cmdHandler.decide st cmd)
```

---

### Task 7: eventium-core — Rename fields in EventSubscription.hs

**Files:**
- Modify: `eventium-core/src/Eventium/EventSubscription.hs`

**Step 1: Rename RetryConfig fields (lines 136-149)**

Before:
```haskell
data RetryConfig = RetryConfig
  { retryInitialDelayMs :: !Int,
    retryMaxDelayMs :: !Int,
    retryBackoffMultiplier :: !Double,
    retryOnError :: SomeException -> IO Bool,
    retryOnErrorCallback :: SomeException -> Int -> IO ()
  }
```

After:
```haskell
data RetryConfig = RetryConfig
  { initialDelayMs :: !Int,
    maxDelayMs :: !Int,
    backoffMultiplier :: !Double,
    onError :: SomeException -> IO Bool,
    onErrorCallback :: SomeException -> Int -> IO ()
  }
```

**Step 2: Update `defaultRetryConfig` (lines 154-161)**

```haskell
defaultRetryConfig =
  RetryConfig
    { initialDelayMs = 1000,
      maxDelayMs = 30000,
      backoffMultiplier = 2.0,
      onError = const (return True),
      onErrorCallback = \_ _ -> return ()
    }
```

**Step 3: Remove `RecordWildCards` pragma and update `pollOnce` (lines 66-73)**

Remove `{-# LANGUAGE RecordWildCards #-}` from line 2.

`pollOnce` uses `getCheckpoint checkpoint` and `saveCheckpoint checkpoint` — these are already unprefixed `CheckpointStore` fields. With `NoFieldSelectors`, they become inaccessible as functions. Use dot syntax:

Before:
```haskell
pollOnce globalReader checkpoint pollIntervalMs handler = do
  latestSeq <- getCheckpoint checkpoint
  newEvents <- getEvents globalReader (eventsStartingAt () $ latestSeq + 1)
  handleEvents handler newEvents
  case NE.nonEmpty newEvents of
    Nothing -> return ()
    Just ne -> saveCheckpoint checkpoint (streamEventPosition $ NE.last ne)
  delayMillis pollIntervalMs
```

After:
```haskell
pollOnce globalReader checkpoint pollIntervalMs handler = do
  latestSeq <- checkpoint.getCheckpoint
  newEvents <- getEvents globalReader (eventsStartingAt () $ latestSeq + 1)
  handleEvents handler newEvents
  case NE.nonEmpty newEvents of
    Nothing -> return ()
    Just ne -> checkpoint.saveCheckpoint ((NE.last ne).position)
  delayMillis pollIntervalMs
```

**Step 4: Update `projectionPollOnce` (lines 104-115)**

Before:
```haskell
projectionPollOnce globalReader proj checkpoint pollIntervalMs = do
  (latestSeq, currentState) <- getCheckpoint checkpoint
  ...
  saveCheckpoint checkpoint (finalSeq, finalState)
  ...
    applyGlobalEvent (_, state) globalEvent =
      let innerEvent = streamEventPayload globalEvent
          newState = projectionEventHandler proj state innerEvent
          newSeq = streamEventPosition globalEvent
       in (newSeq, newState)
```

After:
```haskell
projectionPollOnce globalReader proj checkpoint pollIntervalMs = do
  (latestSeq, currentState) <- checkpoint.getCheckpoint
  ...
  checkpoint.saveCheckpoint (finalSeq, finalState)
  ...
    applyGlobalEvent (_, st) globalEvent =
      let innerEvent = globalEvent.payload
          newState = proj.eventHandler st innerEvent
          newSeq = globalEvent.position
       in (newSeq, newState)
```

**Step 5: Update resilient variants (lines 187-230)**

Replace `retryOnError retryConfig ex` with `retryConfig.onError ex`.
Replace `retryOnErrorCallback retryConfig ex newCount` with `retryConfig.onErrorCallback ex newCount`.

**Step 6: Update `backoffMicros` (lines 232-237)**

Before:
```haskell
backoffMicros RetryConfig {..} count =
  let delayMs =
        min retryMaxDelayMs $
          round (fromIntegral retryInitialDelayMs * retryBackoffMultiplier ^^ max 0 (count - 1))
   in delayMs * 1000
```

After:
```haskell
backoffMicros cfg count =
  let delayMs =
        min cfg.maxDelayMs $
          round (fromIntegral cfg.initialDelayMs * cfg.backoffMultiplier ^^ max 0 (count - 1))
   in delayMs * 1000
```

---

### Task 8: eventium-core — Rename fields in ProcessManager.hs

**Files:**
- Modify: `eventium-core/src/Eventium/ProcessManager.hs`

**Step 1: Rename ProcessManager fields (lines 45-48)**

Before:
```haskell
data ProcessManager state event command = ProcessManager
  { processManagerProjection :: Projection state (VersionedStreamEvent event),
    processManagerReact :: state -> VersionedStreamEvent event -> [ProcessManagerEffect command]
  }
```

After:
```haskell
data ProcessManager state event command = ProcessManager
  { projection :: Projection state (VersionedStreamEvent event),
    react :: state -> VersionedStreamEvent event -> [ProcessManagerEffect command]
  }
```

**Step 2: Remove `RecordWildCards` and update `processManagerEventHandler` (lines 142-146)**

Remove `{-# LANGUAGE RecordWildCards #-}` from line 3.

Before:
```haskell
processManagerEventHandler pm globalReader dispatcher = EventHandler $ \event -> do
  let globalProjection = globalStreamProjection (processManagerProjection pm)
  StreamProjection {..} <- getLatestStreamProjection globalReader globalProjection
  let effects = processManagerReact pm streamProjectionState event
  runProcessManagerEffects dispatcher effects
```

After:
```haskell
processManagerEventHandler pm globalReader dispatcher = EventHandler $ \event -> do
  let globalProjection = globalStreamProjection pm.projection
  sp <- getLatestStreamProjection globalReader globalProjection
  let effects = pm.react sp.state event
  runProcessManagerEffects dispatcher effects
```

**Step 3: Update `dispatchCommand` usage in `runProcessManagerEffects` (lines 118-126)**

`dispatchCommand` is a `CommandDispatcher` field — already unprefixed. With `NoFieldSelectors`, update to dot syntax:

Before:
```haskell
    go (IssueCommand uuid cmd) =
      void $ dispatchCommand dispatcher uuid cmd
    go (IssueCommandWithCompensation uuid cmd onFailure) = do
      result <- dispatchCommand dispatcher uuid cmd
```

After:
```haskell
    go (IssueCommand uuid cmd) =
      void $ dispatcher.dispatchCommand uuid cmd
    go (IssueCommandWithCompensation uuid cmd onFailure) = do
      result <- dispatcher.dispatchCommand uuid cmd
```

---

### Task 9: eventium-core — Update ProjectionCache/Types.hs

**Files:**
- Modify: `eventium-core/src/Eventium/ProjectionCache/Types.hs`

`ProjectionCache` fields (`storeProjectionSnapshot`, `loadProjectionSnapshot`) are already unprefixed. No field renaming needed, but we need to switch from `RecordWildCards` to dot syntax.

**Step 1: Remove `RecordWildCards` pragma (line 2)**

**Step 2: Update `runProjectionCacheUsing` (lines 59-63)**

Before:
```haskell
runProjectionCacheUsing runCache ProjectionCache {..} =
  ProjectionCache
    { storeProjectionSnapshot = \uuid version state -> runCache $ storeProjectionSnapshot uuid version state,
      loadProjectionSnapshot = runCache . loadProjectionSnapshot
    }
```

After:
```haskell
runProjectionCacheUsing runCache cache =
  ProjectionCache
    { storeProjectionSnapshot = \k pos st -> runCache $ cache.storeProjectionSnapshot k pos st,
      loadProjectionSnapshot = runCache . cache.loadProjectionSnapshot
    }
```

**Step 3: Update `codecProjectionCache` (lines 73-79)**

Before:
```haskell
codecProjectionCache codec ProjectionCache {..} =
  ProjectionCache storeProjectionSnapshot' loadProjectionSnapshot'
  where
    storeProjectionSnapshot' uuid version = storeProjectionSnapshot uuid version . encode codec
    loadProjectionSnapshot' uuid = do
      mState <- loadProjectionSnapshot uuid
      return $ mState >>= traverse (decode codec)
```

After:
```haskell
codecProjectionCache codec cache =
  ProjectionCache storeProjectionSnapshot' loadProjectionSnapshot'
  where
    storeProjectionSnapshot' k pos = cache.storeProjectionSnapshot k pos . codec.encode
    loadProjectionSnapshot' k = do
      mState <- cache.loadProjectionSnapshot k
      return $ mState >>= traverse codec.decode
```

**Step 4: Update `getLatestProjectionWithCache'` (lines 110-120)**

Before:
```haskell
getLatestProjectionWithCache' cache projection key = do
  mLatestState <- loadProjectionSnapshot cache key
  let mkProjection' (position, state) =
        if position > streamProjectionPosition projection
          then
            projection
              { streamProjectionPosition = position,
                streamProjectionState = state
              }
          else projection
  return $ maybe projection mkProjection' mLatestState
```

After:
```haskell
getLatestProjectionWithCache' cache proj k = do
  mLatestState <- cache.loadProjectionSnapshot k
  let mkProjection' (pos, st) =
        if pos > proj.position
          then proj { position = pos, state = st }
          else proj
  return $ maybe proj mkProjection' mLatestState
```

**Step 5: Update `updateProjectionCache` (lines 130-132)**

Before:
```haskell
updateProjectionCache reader cache projection = do
  StreamProjection {..} <- getLatestVersionedProjectionWithCache reader cache projection
  storeProjectionSnapshot cache streamProjectionKey streamProjectionPosition streamProjectionState
```

After:
```haskell
updateProjectionCache reader cache proj = do
  sp <- getLatestVersionedProjectionWithCache reader cache proj
  cache.storeProjectionSnapshot sp.key sp.position sp.state
```

**Step 6: Update `updateGlobalProjectionCache` (lines 142-144)**

Before:
```haskell
updateGlobalProjectionCache reader cache projection key = do
  StreamProjection {..} <- getLatestGlobalProjectionWithCache reader cache projection key
  storeProjectionSnapshot cache key streamProjectionPosition streamProjectionState
```

After:
```haskell
updateGlobalProjectionCache reader cache proj k = do
  sp <- getLatestGlobalProjectionWithCache reader cache proj k
  cache.storeProjectionSnapshot k sp.position sp.state
```

---

### Task 10: eventium-core — Update TypeEmbedding.hs

**Files:**
- Modify: `eventium-core/src/Eventium/TypeEmbedding.hs`

`TypeEmbedding` fields (`embed`, `extract`) are already unprefixed. With `NoFieldSelectors`, they can no longer be used as top-level functions. Update all usages in the file to dot syntax.

Check the `composeTypeEmbeddings` function or similar. Based on exploration:

```haskell
-- Before
{ embed = embed emb2 . embed emb1,
  extract = extract emb2 >=> extract emb1
}
-- After
{ embed = emb2.embed . emb1.embed,
  extract = emb2.extract >=> emb1.extract
}
```

---

### Task 11: eventium-core — Update remaining internal call sites

**Files:**
- Modify: `eventium-core/src/Eventium/EventPublisher.hs`
- Modify: `eventium-core/src/Eventium/EventHandler.hs`

`EventPublisher` has field `publishEvents` and `EventHandler` has `handleEvent` — already unprefixed. With `NoFieldSelectors`, these can't be used as top-level functions. Update internal usages:

In `EventPublisher.hs`:
- `publish uuid versionedEvents` → check if `publish` comes from RecordWildCards or a local binding. Read the file and update accordingly. Replace any `publishEvents publisher ...` with `publisher.publishEvents ...`.

In `EventHandler.hs`:
- `handleEvents (EventHandler h) = mapM_ h` — this pattern matches the constructor, not the field name. Should still work. But `handleEvent` may be used elsewhere — with `NoFieldSelectors`, it can no longer be a top-level selector. Update callers to use dot syntax or pattern matching.

Also check `EventSubscription.hs`:
- `runSubscription` field of `EventSubscription` — update callers to dot syntax.

---

### Task 12: eventium-core — Update tests

**Files:**
- Modify: `eventium-core/tests/Eventium/ProjectionSpec.hs`
- Modify: `eventium-core/tests/Eventium/CommandDispatcherSpec.hs`
- Modify: other test files in `eventium-core/tests/`

**Step 1: Update ProjectionSpec.hs**

Line 14: `Projection 0 (+)` — positional construction, no change needed.

Lines 70-72:
```haskell
-- Before
streamProjectionState sp' `shouldBe` 10
streamProjectionPosition sp' `shouldBe` 5
streamProjectionKey sp' `shouldBe` "key"
-- After
sp'.state `shouldBe` 10
sp'.position `shouldBe` 5
sp'.key `shouldBe` "key"
```

Line 57: `(\(DecodeError ctx _) -> ctx == "codecProjection")` — uses positional pattern matching on `DecodeError`. Still works, but the field was renamed from `decodeErrorContext` to `context`. Positional patterns don't use field names, so no change needed.

**Step 2: Update CommandDispatcherSpec.hs**

Line 54: `queryRangeKey query` — update to `query.key`.

Line 8: `dispatchCommand` import from `Eventium.ProcessManager` — this was a field accessor, now it's not a top-level function with `NoFieldSelectors`. Update usage at lines 67, 77, 87 to use dot syntax on the dispatcher object.

But wait — `dispatchCommand` is used in the spec as `dispatchCommand dispatcher (uuidFromInteger 1) Increment`. With `NoFieldSelectors`, this won't work. Update to `dispatcher.dispatchCommand (uuidFromInteger 1) Increment`.

---

### Task 13: eventium-core — Build and test

**Step 1: Build**

Run: `cabal build eventium-core`

Fix any compilation errors. Common issues:
- Missed field accessor → dot syntax conversions
- Ambiguous field names (should be resolved by `NoFieldSelectors`)
- Record update syntax needs the new field names

**Step 2: Test**

Run: `cabal test eventium-core`

All tests should pass.

**Step 3: Commit**

```bash
git add eventium-core/
git commit -m "refactor(core): remove record field prefixes

Enable NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot.
Rename all prefixed record fields to short names. Replace
RecordWildCards with dot syntax. Update JSON instances to use
defaultOptions."
```

---

### Task 14: eventium-memory — Enable extensions and update

**Files:**
- Modify: `eventium-memory/package.yaml` — add same `default-extensions` block
- Modify: `eventium-memory/src/Eventium/Store/Memory.hs` — internal `EventMap` type uses `_eventMapUuidMap` prefix (private, not exported). Rename or leave as-is since it's internal and already uses positional pattern matching.
- Modify: `eventium-memory/src/Eventium/ProjectionCache/Memory.hs` — remove `RecordWildCards`, use named record construction for `ProjectionCache`
- Modify: `eventium-memory/tests/MemoryTestImport.hs` — rename `_embeddedDummyArgument` → `dummyArgument`, `embeddedEventMap` → `eventMap`, `embeddedProjectionMap` → `projectionMap`. Update `setEventMap` and `setProjectionMap` record updates.
- Modify: `eventium-memory/tests/Eventium/Store/MemorySpec.hs` — update field accessor calls
- Modify: `eventium-memory/tests/Eventium/ProjectionCache/MemorySpec.hs` — update field accessor calls
- Modify: other test files that use core type accessors (e.g., `streamProjectionState` → `.state`)

**Step 1: Update package.yaml**

Add `default-extensions` under both `library:` and `tests:`:
```yaml
default-extensions:
  - NoFieldSelectors
  - DuplicateRecordFields
  - OverloadedRecordDot
```

**Step 2: Update Memory.hs**

`EventMap` fields `_eventMapUuidMap` and `_eventMapGlobalEvents` are private/internal. They're accessed via positional pattern matching. Leave as-is or rename — implementor's choice. Since they have underscore prefix (suggesting they shouldn't be accessed directly), consider removing the prefix to just `uuidMap` and `globalEvents`. Update the 3-4 functions that pattern match on `EventMap`.

**Step 3: Update ProjectionCache/Memory.hs**

Remove `RecordWildCards`. Replace `ProjectionCache {..}` construction with named fields:

```haskell
-- Before
tvarProjectionCache tvar =
  let storeProjectionSnapshot uuid version projState = ...
      loadProjectionSnapshot uuid = ...
   in ProjectionCache {..}
-- After
tvarProjectionCache tvar =
  ProjectionCache
    { storeProjectionSnapshot = \uuid version projState -> ...,
      loadProjectionSnapshot = \uuid -> ...
    }
```

**Step 4: Update test files**

Replace all `streamProjectionState x` with `x.state`, `streamProjectionPosition x` with `x.position`, etc.
Replace all `streamEventPayload x` with `x.payload`, `streamEventPosition x` with `x.position`, `streamEventKey x` with `x.key`.
Replace `storeProjectionSnapshot cache ...` with `cache.storeProjectionSnapshot ...`.
Replace `loadProjectionSnapshot cache ...` with `cache.loadProjectionSnapshot ...`.

**Step 5: Build and test**

Run: `cabal build eventium-memory && cabal test eventium-memory`

**Step 6: Commit**

```bash
git add eventium-memory/
git commit -m "refactor(memory): remove record field prefixes"
```

---

### Task 15: eventium-sql-common — Enable extensions and update

**Files:**
- Modify: `eventium-sql-common/package.yaml`
- Modify: `eventium-sql-common/src/Eventium/Store/Sql/Operations.hs`
- Modify: `eventium-sql-common/src/Eventium/Store/Sql/DefaultEntity.hs`

**Step 1: Update package.yaml**

Add `default-extensions` under `library:`:
```yaml
default-extensions:
  - NoFieldSelectors
  - DuplicateRecordFields
  - OverloadedRecordDot
```

**Step 2: Rename SqlEventStoreConfig fields (Operations.hs lines 33-49)**

Strip `sqlEventStoreConfig` prefix:

| Old | New |
|-----|-----|
| `sqlEventStoreConfigSequenceMakeEntity` | `sequenceMakeEntity` |
| `sqlEventStoreConfigMakeKey` | `makeKey` |
| `sqlEventStoreConfigUnKey` | `unKey` |
| `sqlEventStoreConfigUUID` | `uuid` |
| `sqlEventStoreConfigVersion` | `version` |
| `sqlEventStoreConfigData` | `eventData` |
| `sqlEventStoreConfigMetadata` | `eventMetadata` |
| `sqlEventStoreConfigSequenceNumberField` | `sequenceNumberField` |
| `sqlEventStoreConfigUUIDField` | `uuidField` |
| `sqlEventStoreConfigVersionField` | `versionField` |
| `sqlEventStoreConfigDataField` | `dataField` |

Note: `data` is a Haskell keyword, so use `eventData` instead. Similarly, `metadata` could clash with `StreamEvent.metadata` — use `eventMetadata` to be specific.

**Step 3: Remove `RecordWildCards` and update all functions**

This file heavily uses `SqlEventStoreConfig {..}` pattern matching. Replace with `config` parameter and dot syntax throughout.

Example for `sqlEventToVersioned` (lines 81-86):

Before:
```haskell
sqlEventToVersioned SqlEventStoreConfig {..} entity =
  StreamEvent
    (sqlEventStoreConfigUUID entity)
    (sqlEventStoreConfigVersion entity)
    (decodeMetadata $ sqlEventStoreConfigMetadata entity)
    (sqlEventStoreConfigData entity)
```

After:
```haskell
sqlEventToVersioned config entity =
  StreamEvent
    (config.uuid entity)
    (config.version entity)
    (decodeMetadata $ config.eventMetadata entity)
    (config.eventData entity)
```

Apply similar pattern to all other functions: `sqlGetProjectionIds`, `sqlGetStreamEvents`, `sqlGetAllEventsInRange`, `sqlMaxEventVersion`, `sqlStoreEvents`, `sqlStoreEventsTagged`, `unsafeSqlStoreGlobalStreamEvents`.

Also update `QueryRange {..}` destructuring in `sqlGetStreamEvents` and `sqlGetAllEventsInRange`:

Before:
```haskell
sqlGetStreamEvents config@SqlEventStoreConfig {..} QueryRange {..} = do
  ...
  case queryRangeStart of ...
  (sqlEventStoreConfigUUIDField ==. queryRangeKey) : ...
```

After:
```haskell
sqlGetStreamEvents config query = do
  ...
  case query.start of ...
  (config.uuidField ==. query.key) : ...
```

**Step 4: Update DefaultEntity.hs (lines 54-67)**

Update `defaultSqlEventStoreConfig` record construction with new field names.

**Step 5: Build and test**

Run: `cabal build eventium-sql-common`

(No tests in this package directly — tested via postgresql and sqlite.)

**Step 6: Commit**

```bash
git add eventium-sql-common/
git commit -m "refactor(sql-common): remove record field prefixes from SqlEventStoreConfig"
```

---

### Task 16: eventium-postgresql and eventium-sqlite — Enable extensions and update

**Files:**
- Modify: `eventium-postgresql/package.yaml`
- Modify: `eventium-postgresql/src/Eventium/Store/Postgresql.hs`
- Modify: `eventium-postgresql/tests/Eventium/Store/PostgresqlSpec.hs`
- Modify: `eventium-sqlite/package.yaml`
- Modify: `eventium-sqlite/src/Eventium/Store/Sqlite.hs`
- Modify: `eventium-sqlite/tests/Eventium/Store/SqliteSpec.hs`

**Step 1: Update both package.yaml files**

Add `default-extensions` under `library:` and `tests:` in both.

**Step 2: Update Postgresql.hs**

Minimal changes — this file passes `config` objects to sql-common functions. No RecordWildCards here. Just ensure any direct field accesses use dot syntax.

**Step 3: Update Sqlite.hs**

Remove `RecordWildCards` pragma. Update `initializeSqliteEventStore` which uses `SqlEventStoreConfig {..}`:

Before:
```haskell
initializeSqliteEventStore SqlEventStoreConfig {..} pool = do
  ...
  let tableName = unEntityNameDB $ tableDBName (sqlEventStoreConfigSequenceMakeEntity undefined ...)
      uuidFieldName = unFieldNameDB $ fieldDBName sqlEventStoreConfigSequenceNumberField
```

After:
```haskell
initializeSqliteEventStore config pool = do
  ...
  let tableName = unEntityNameDB $ tableDBName (config.sequenceMakeEntity undefined ...)
      uuidFieldName = unFieldNameDB $ fieldDBName config.sequenceNumberField
```

**Step 4: Update test files**

Replace `streamProjectionState`, `streamProjectionPosition`, `streamEventPayload`, etc. with dot syntax. These tests likely use the testkit, so changes may be minimal.

**Step 5: Build and test**

Run: `cabal build eventium-postgresql eventium-sqlite`
Run: `cabal test eventium-sqlite`
Run: `cabal test eventium-postgresql` (requires Postgres — skip if not available)

**Step 6: Commit**

```bash
git add eventium-postgresql/ eventium-sqlite/
git commit -m "refactor(postgresql, sqlite): remove record field prefixes"
```

---

### Task 17: eventium-testkit — Enable extensions and update

**Files:**
- Modify: `eventium-testkit/package.yaml`
- Modify: `eventium-testkit/src/Eventium/Testkit.hs`

**Step 1: Update package.yaml**

Add `default-extensions` under `library:`.

**Step 2: Rename CounterEvent fields (lines 46-51)**

Before:
```haskell
data CounterEvent
  = Added
      { _counterEventAmount :: Int
      }
  | CounterFailedOutOfBounds
```

After:
```haskell
data CounterEvent
  = Added
      { amount :: Int
      }
  | CounterFailedOutOfBounds
```

**Step 3: Rename CounterCommand fields (lines 67-74)**

Before:
```haskell
data CounterCommand
  = Increment
      { _counterCommandAmount :: Int
      }
  | Decrement
      { _counterCommandAmount :: Int
      }
```

After:
```haskell
data CounterCommand
  = Increment
      { amount :: Int
      }
  | Decrement
      { amount :: Int
      }
```

**Step 4: Update JSON derivation (lines 95-96)**

Before:
```haskell
deriveJSON (aesonPrefix camelCase) ''CounterEvent
deriveJSON (aesonPrefix camelCase) ''CounterCommand
```

After:
```haskell
deriveJSON defaultOptions ''CounterEvent
deriveJSON defaultOptions ''CounterCommand
```

Check that the JSON output matches. `aesonPrefix camelCase` with `_counterEventAmount` → strips prefix → `amount` → camelCase → `amount`. With `defaultOptions` and field name `amount` → `amount`. Same result. Good.

Remove `Data.Aeson.Casing` import and add `Data.Aeson (defaultOptions)` if needed. The `aeson-casing` dependency can be removed from `package.yaml` if no longer used.

**Step 5: Update all field accessor usage to dot syntax**

Replace throughout the file:
- `streamProjectionState x` → `x.state`
- `streamProjectionPosition x` → `x.position`
- `streamProjectionKey x` → `x.key`
- `streamEventPayload x` → `x.payload`
- `streamEventPosition x` → `x.position`
- `streamEventKey x` → `x.key`
- `streamEventPayload . streamEventPayload` → `(.payload) . (.payload)` or inline

For composed accessors like `streamEventPayload . streamEventPayload <$> events`, use:
```haskell
((.payload) . (.payload)) <$> events
```
Or inline: `(\e -> e.payload.payload) <$> events`

**Step 6: Update `allCommandHandlerStates` (lines 116-121)**

Before:
```haskell
allCommandHandlerStates (CommandHandler commandHandler (Projection seed eventHandler)) =
  scanl' go seed
  where
    go state command = case commandHandler state command of
      Left _ -> state
      Right events -> foldl' eventHandler state events
```

After:
```haskell
allCommandHandlerStates cmdHandler =
  scanl' go cmdHandler.projection.seed
  where
    go st command = case cmdHandler.decide st command of
      Left _ -> st
      Right events -> foldl' cmdHandler.projection.eventHandler st events
```

**Step 7: Update `counterProjection` (lines 56-59) and `counterGlobalProjection` (lines 62-65)**

These use positional `Projection` and `StreamEvent` construction — no change needed for construction. But the lambda pattern matching on `StreamEvent _ _ _ (Added x)` uses positional — still works.

**Step 8: Build and test**

Run: `cabal build eventium-testkit`

Then run downstream tests that use testkit:
Run: `cabal test eventium-memory`

**Step 9: Commit**

```bash
git add eventium-testkit/
git commit -m "refactor(testkit): remove record field prefixes"
```

---

### Task 18: examples-bank — Enable extensions and remove lenses

**Files:**
- Modify: `examples/bank/package.yaml`
- Modify: `examples/bank/src/Bank/Models/Account/Projection.hs`
- Modify: `examples/bank/src/Bank/Models/Account/Events.hs`
- Modify: `examples/bank/src/Bank/Models/Account/CommandHandler.hs`
- Modify: `examples/bank/src/Bank/Models/Customer/Projection.hs`
- Modify: `examples/bank/src/Bank/Models/Customer/Events.hs`
- Modify: `examples/bank/src/Bank/Models/Customer/Commands.hs`
- Modify: `examples/bank/src/Bank/ReadModels/CustomerAccounts.hs`
- Modify: `examples/bank/src/Bank/ProcessManagers/TransferManager.hs`
- Modify: `examples/bank/src/Bank/CLI.hs`
- Modify: `examples/bank/src/Bank/CLI/Options.hs`
- Modify: `examples/bank/src/Bank/CLI/Store.hs`

**Step 1: Update package.yaml**

Add `default-extensions` under `library:`, `executables:`, and `tests:`.
Remove `lens` from dependencies.

**Step 2: Rename Account fields (Account/Projection.hs)**

Before:
```haskell
data Account = Account
  { _accountBalance :: Double,
    _accountOwner :: Maybe UUID,
    _accountPendingTransfers :: [PendingAccountTransfer]
  }
```

After:
```haskell
data Account = Account
  { balance :: Double,
    owner :: Maybe UUID,
    pendingTransfers :: [PendingAccountTransfer]
  }
```

Remove `makeLenses ''Account`.

**Step 3: Rename PendingAccountTransfer fields**

Before:
```haskell
data PendingAccountTransfer = PendingAccountTransfer
  { pendingAccountTransferId :: UUID,
    pendingAccountTransferAmount :: Double,
    pendingAccountTransferTargetAccount :: UUID
  }
```

After:
```haskell
data PendingAccountTransfer = PendingAccountTransfer
  { id :: UUID,
    amount :: Double,
    targetAccount :: UUID
  }
```

**Step 4: Replace lens operations in handleAccountEvent**

Replace `account ^. accountBalance` → `account.balance`.
Replace `account & accountBalance .~ x` → `account { balance = x }`.
Replace `account & accountBalance +~ x` → `account { balance = account.balance + x }`.
Replace `account & accountBalance -~ x` → `account { balance = account.balance - x }`.
Replace `account & accountPendingTransfers %~ f` → `account { pendingTransfers = f account.pendingTransfers }`.
Replace `account & accountOwner ?~ x` → `account { owner = Just x }`.

**Step 5: Rename all event types in Events.hs**

Strip type-name prefixes from all event record fields:
- `accountOpenedOwner` → `owner`
- `accountOpenedInitialFunding` → `initialFunding`
- `accountCreditedAmount` → `amount`
- `accountCreditedReason` → `reason`
- `accountDebitedAmount` → `amount`
- `accountDebitedReason` → `reason`
- `accountTransferStartedTransferId` → `transferId`
- `accountTransferStartedAmount` → `amount`
- `accountTransferStartedTargetAccount` → `targetAccount`
- `accountTransferCompletedTransferId` → `transferId`
- `accountTransferFailedTransferId` → `transferId`
- `accountTransferFailedReason` → `reason`
- `accountCreditedFromTransferTransferId` → `transferId`
- `accountCreditedFromTransferSourceAccount` → `sourceAccount`
- `accountCreditedFromTransferAmount` → `amount`

**Step 6: Update CommandHandler.hs**

Remove `RecordWildCards`. Replace all `{..}` pattern matches with explicit field patterns or positional patterns. Update record construction with new field names.

**Step 7: Update CustomerAccounts.hs**

Rename fields, remove `RecordWildCards`, remove `makeLenses`, replace lens operations with dot syntax and record updates.

**Step 8: Update TransferManager.hs**

Rename `_transferManagerData` → `transferData`. Remove `makeLenses`. Replace lens operations.

**Step 9: Update Customer/Projection.hs, Customer/Events.hs, Customer/Commands.hs**

Rename: `customerName` → `name`, `customerCreatedName` → `name`, `customerCreationRejectedReason` → `reason`, `createCustomerData` → `name`.

**Step 10: Update CLI.hs, CLI/Options.hs, CLI/Store.hs**

Remove `RecordWildCards`. Update field accesses to dot syntax. Rename `optionsDatabaseFile` → `databaseFile`, `optionsCommand` → `command`.

**Step 11: Remove lens imports**

Remove all `Control.Lens` imports across the bank example.

**Step 12: Build and test**

Run: `cabal build examples-bank && cabal test examples-bank`

**Step 13: Commit**

```bash
git add examples/bank/
git commit -m "refactor(examples/bank): remove record field prefixes and lens dependency"
```

---

### Task 19: examples-cafe — Enable extensions and remove lenses

**Files:**
- Modify: `examples/cafe/package.yaml`
- Modify: `examples/cafe/src/Cafe/Models/Tab.hs`
- Modify: `examples/cafe/src/Cafe/ChefTodoList.hs`
- Modify: `examples/cafe/src/Cafe/CLI.hs`
- Modify: `examples/cafe/src/Cafe/CLI/Options.hs`

**Step 1: Update package.yaml**

Add `default-extensions` under `library:` and `executables:`.
Remove `lens` from dependencies.

**Step 2: Rename Tab.hs fields**

- `menuItemDescription` → `description`
- `menuItemPrice` → `price`
- `_tabStateIsOpen` → `isOpen`
- `_tabStateOutstandingDrinks` → `outstandingDrinks`
- `_tabStateOutstandingFood` → `outstandingFood`
- `_tabStatePreparedFood` → `preparedFood`
- `_tabStateServedItems` → `servedItems`
- `_tabEventDrinks` → `drinks`
- `_tabEventFood` → `food`
- `_tabEventDrinkIndexes` → `drinkIndexes`
- `_tabEventFoodIndexes` → `foodIndexes`

Remove `makeLenses` calls.

**Step 3: Replace lens operations in Tab.hs**

Replace `state ^. tabStateOutstandingDrinks` → `state.outstandingDrinks`.
Replace `state & tabStateOutstandingDrinks %~ f` → `state { outstandingDrinks = f state.outstandingDrinks }`.
Similar for all other lens operations.

**Step 4: Update ChefTodoList.hs**

Replace `streamEventKey streamEvent` → `streamEvent.key`.
Replace `streamEventPayload streamEvent` → `streamEvent.payload`.

**Step 5: Update CLI.hs and CLI/Options.hs**

Remove `RecordWildCards`. Rename `optionsDatabaseFile` → `databaseFile`, `optionsCommand` → `command`. Use dot syntax.
Replace `StreamProjection {..}` pattern with named binding.

**Step 6: Build and test**

Run: `cabal build examples-cafe`

(Cafe has no test suite.)

**Step 7: Commit**

```bash
git add examples/cafe/
git commit -m "refactor(examples/cafe): remove record field prefixes and lens dependency"
```

---

### Task 20: examples-counter-cli — Enable extensions and update

**Files:**
- Modify: `examples/counter-cli/package.yaml`
- Modify: `examples/counter-cli/counter-cli.hs`

**Step 1: Update package.yaml**

Add `default-extensions` under `executables:`.

**Step 2: Update counter-cli.hs**

Line 29: `streamProjectionState latestStreamProjection` → `latestStreamProjection.state`

`CounterState` and `CounterEvent` fields — rename only if prefixed. `unCounterState` is a newtype unwrapper — keep as-is per design decision. `CounterEvent` constructors don't have record fields (they use positional). No rename needed.

**Step 3: Build**

Run: `cabal build examples-counter-cli`

**Step 4: Commit**

```bash
git add examples/counter-cli/
git commit -m "refactor(examples/counter-cli): remove record field prefixes"
```

---

### Task 21: Full build and test

**Step 1: Regenerate all cabal files**

Run: `just hpack`

**Step 2: Full build**

Run: `just build`

**Step 3: Full test**

Run: `just test`

(PostgreSQL tests may be skipped if Docker isn't running.)

**Step 4: Format**

Run: `just format`

**Step 5: Lint**

Run: `hlint eventium-core/src eventium-memory/src eventium-sql-common/src eventium-postgresql/src eventium-sqlite/src eventium-testkit/src examples/`

**Step 6: Final commit (if formatting changes)**

```bash
git add -A
git commit -m "style: format after record field prefix removal"
```

---

### Task 22: Clean up Eventium.Json module

**Files:**
- Modify: `eventium-core/src/Eventium/Json.hs`

After the migration, check if `unPrefixLower` is still used anywhere. If `EventMetadata` was the only consumer and it now uses `defaultOptions`, consider whether to keep or remove `unPrefixLower`. If it's still re-exported or used in examples, keep it. Otherwise, remove the unused function to keep the codebase clean.

Run: `grep -r "unPrefixLower" .` to check usage.

If unused, remove it and update the module exports. Build and test again.
