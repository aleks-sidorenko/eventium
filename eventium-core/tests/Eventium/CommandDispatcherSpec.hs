{-# LANGUAGE OverloadedStrings #-}

module Eventium.CommandDispatcherSpec (spec) where

import Data.IORef
import Eventium.Codec
import Eventium.CommandDispatcher
import Eventium.CommandHandler
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher (..), RejectionReason (..))
import Eventium.Projection
import Eventium.Store.Class
import Eventium.UUID
import Test.Hspec

-- Minimal test domain
data CounterEvent = Incremented | Decremented deriving (Show, Eq)

data CounterCommand = Increment | Decrement | Unknown deriving (Show, Eq)

data CounterError = AlreadyZero deriving (Show, Eq)

type Counter = Int

counterProjection :: Projection Counter CounterEvent
counterProjection =
  Projection 0 $ \s e -> case e of
    Incremented -> s + 1
    Decremented -> s - 1

counterHandler :: CommandHandler Counter CounterEvent CounterCommand CounterError
counterHandler = CommandHandler decide' counterProjection
  where
    decide' _ Increment = Right [Incremented]
    decide' s Decrement
      | s <= 0 = Left AlreadyZero
      | otherwise = Right [Decremented]
    decide' _ Unknown = Right []

testCodec :: Codec CounterEvent CounterEvent
testCodec = Codec id Just

-- | Simple IORef-based event store for testing.
-- Returns a tagged writer, a reader that reads domain events, and raw reader.
mkTestStore :: IO (VersionedEventStoreWriter IO (TaggedEvent CounterEvent), VersionedEventStoreReader IO CounterEvent)
mkTestStore = do
  eventsRef <- newIORef ([] :: [VersionedStreamEvent CounterEvent])
  let taggedWriter = EventStoreWriter $ \uuid _expected taggedEvents -> do
        existing <- readIORef eventsRef
        let events = map (.payload) taggedEvents
            startVersion = fromIntegral (length existing)
            versioned = zipWith (\i e -> StreamEvent uuid i (emptyMetadata "") e) [startVersion ..] events
            poss = take (length events) [SequenceNumber (length existing + 1) ..]
        modifyIORef eventsRef (++ versioned)
        pure (Right (zip [startVersion ..] poss))
      reader = EventStoreReader $ \query -> do
        allEvts <- readIORef eventsRef
        pure $ filterByQuery query allEvts
  pure (taggedWriter, reader)
  where
    filterByQuery (QueryRange uuid _ _) =
      filter (\(StreamEvent k _ _ _) -> k == uuid)

-- | Like 'mkTestStore', but also captures every 'TaggedEvent' passed to the
-- writer (metadata included) so tests can assert on the tag actually used.
mkCapturingTestStore ::
  IO
    ( IORef [TaggedEvent CounterEvent],
      VersionedEventStoreWriter IO (TaggedEvent CounterEvent),
      VersionedEventStoreReader IO CounterEvent
    )
mkCapturingTestStore = do
  eventsRef <- newIORef ([] :: [VersionedStreamEvent CounterEvent])
  capturedRef <- newIORef ([] :: [TaggedEvent CounterEvent])
  let taggedWriter = EventStoreWriter $ \uuid _expected taggedEvents -> do
        modifyIORef capturedRef (++ taggedEvents)
        existing <- readIORef eventsRef
        let events = map (.payload) taggedEvents
            startVersion = fromIntegral (length existing)
            versioned = zipWith (\i e -> StreamEvent uuid i (emptyMetadata "") e) [startVersion ..] events
            poss = take (length events) [SequenceNumber (length existing + 1) ..]
        modifyIORef eventsRef (++ versioned)
        pure (Right (zip [startVersion ..] poss))
      reader = EventStoreReader $ \query -> do
        allEvts <- readIORef eventsRef
        pure $ filterByQuery query allEvts
  pure (capturedRef, taggedWriter, reader)
  where
    filterByQuery (QueryRange uuid _ _) =
      filter (\(StreamEvent k _ _ _) -> k == uuid)

spec :: Spec
spec = describe "CommandDispatcher" $ do
  describe "commandHandlerDispatcher" $ do
    it "routes command to matching handler and reports success" $ do
      (taggedWriter, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher testCodec taggedWriter reader handlers

      result <- dispatcher.dispatchCommand (uuidFromInteger 1) Increment id
      result `shouldBe` CommandSucceeded

    it "reports failure when command is rejected" $ do
      (taggedWriter, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher testCodec taggedWriter reader handlers

      -- Counter starts at 0, Decrement should fail
      result <- dispatcher.dispatchCommand (uuidFromInteger 1) Decrement id
      result `shouldBe` CommandFailed (RejectionReason "AlreadyZero")

    it "returns CommandSucceeded when no handler matches" $ do
      (taggedWriter, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher testCodec taggedWriter reader handlers

      -- Unknown returns Right [], so no handler "matches" (produces events)
      result <- dispatcher.dispatchCommand (uuidFromInteger 1) Unknown id
      result `shouldBe` CommandSucceeded

    it "tags emitted events with the Typeable event type name" $ do
      (capturedRef, taggedWriter, reader) <- mkCapturingTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher testCodec taggedWriter reader handlers

      _ <- dispatcher.dispatchCommand (uuidFromInteger 1) Increment id
      captured <- readIORef capturedRef
      map (.metadata.eventType) captured `shouldBe` ["CounterEvent"]

  describe "commandHandlerDispatcherWithTag" $ do
    it "tags emitted events with the caller-supplied event type name" $ do
      (capturedRef, taggedWriter, reader) <- mkCapturingTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher =
            commandHandlerDispatcherWithTag (const "SpecificTag") testCodec taggedWriter reader handlers

      result <- dispatcher.dispatchCommand (uuidFromInteger 1) Increment id
      result `shouldBe` CommandSucceeded

      captured <- readIORef capturedRef
      map (.metadata.eventType) captured `shouldBe` ["SpecificTag"]
