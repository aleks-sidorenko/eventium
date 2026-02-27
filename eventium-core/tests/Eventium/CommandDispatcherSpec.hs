{-# LANGUAGE OverloadedStrings #-}

module Eventium.CommandDispatcherSpec (spec) where

import Data.IORef
import Eventium.CommandDispatcher
import Eventium.CommandHandler
import Eventium.ProcessManager (CommandDispatchResult (..), RejectionReason (..), dispatchCommand)
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
counterHandler = CommandHandler decide counterProjection
  where
    decide _ Increment = Right [Incremented]
    decide s Decrement
      | s <= 0 = Left AlreadyZero
      | otherwise = Right [Decremented]
    decide _ Unknown = Right []

-- | Simple IORef-based event store for testing.
mkTestStore :: IO (VersionedEventStoreWriter IO CounterEvent, VersionedEventStoreReader IO CounterEvent)
mkTestStore = do
  eventsRef <- newIORef ([] :: [VersionedStreamEvent CounterEvent])
  let writer = EventStoreWriter $ \uuid _expected events -> do
        existing <- readIORef eventsRef
        let startVersion = fromIntegral (length existing)
            versioned = zipWith (\i e -> StreamEvent uuid i (emptyMetadata "") e) [startVersion ..] events
        modifyIORef eventsRef (++ versioned)
        pure (Right (startVersion + fromIntegral (length events) - 1))
      reader = EventStoreReader $ \query -> do
        allEvts <- readIORef eventsRef
        pure $ filterByQuery query allEvts
  pure (writer, reader)
  where
    filterByQuery query events =
      case queryRangeKey query of
        uuid ->
          filter (\(StreamEvent k _ _ _) -> k == uuid) events

spec :: Spec
spec = describe "CommandDispatcher" $ do
  describe "commandHandlerDispatcher" $ do
    it "routes command to matching handler and reports success" $ do
      (writer, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher writer reader handlers

      result <- dispatchCommand dispatcher (uuidFromInteger 1) Increment
      result `shouldBe` CommandSucceeded

    it "reports failure when command is rejected" $ do
      (writer, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher writer reader handlers

      -- Counter starts at 0, Decrement should fail
      result <- dispatchCommand dispatcher (uuidFromInteger 1) Decrement
      result `shouldBe` CommandFailed (RejectionReason "AlreadyZero")

    it "returns CommandSucceeded when no handler matches" $ do
      (writer, reader) <- mkTestStore

      let handlers = [mkAggregateHandler counterHandler]
          dispatcher = commandHandlerDispatcher writer reader handlers

      -- Unknown returns Right [], so no handler "matches" (produces events)
      result <- dispatchCommand dispatcher (uuidFromInteger 1) Unknown
      result `shouldBe` CommandSucceeded
