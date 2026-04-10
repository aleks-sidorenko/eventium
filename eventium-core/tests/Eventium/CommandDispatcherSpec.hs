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
        modifyIORef eventsRef (++ versioned)
        pure (Right (startVersion + fromIntegral (length events) - 1))
      reader = EventStoreReader $ \query -> do
        allEvts <- readIORef eventsRef
        pure $ filterByQuery query allEvts
  pure (taggedWriter, reader)
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
