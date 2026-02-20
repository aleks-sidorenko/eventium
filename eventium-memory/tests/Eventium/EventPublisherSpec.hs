{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eventium.EventPublisherSpec (spec) where

import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Eventium.EventHandler
import Eventium.EventPublisher
import Eventium.Store.Class
import Eventium.Store.Memory
import Eventium.UUID
import Test.Hspec

-- Helper to create IO-compatible stores from a TVar
makeIOStore :: TVar (EventMap event) -> (VersionedEventStoreWriter IO event, VersionedEventStoreReader IO event, GlobalEventStoreReader IO event)
makeIOStore tvar =
  ( runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar),
    runEventStoreReaderUsing atomically (tvarEventStoreReader tvar),
    runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)
  )

spec :: Spec
spec = do
  describe "synchronousPublisher" $ do
    it "should deliver each event to the handler" $ do
      ref <- newIORef ([] :: [Int])
      let handler = EventHandler $ \(StreamEvent _ _ _ e) -> modifyIORef ref (++ [e])
          publisher = synchronousPublisher handler
      publishEvents
        publisher
        nil
        [ StreamEvent nil 0 (emptyMetadata "") (1 :: Int),
          StreamEvent nil 1 (emptyMetadata "") 2,
          StreamEvent nil 2 (emptyMetadata "") 3
        ]
      readIORef ref `shouldReturn` [1, 2, 3]

  describe "publishingEventStoreWriter" $ do
    it "should publish events after a successful write" $ do
      tvar <- eventMapTVar
      let (writer, reader, _) = makeIOStore tvar

      ref <- newIORef ([] :: [Int])
      let handler = EventHandler $ \(StreamEvent _ _ _ e) -> modifyIORef ref (++ [e])
          publisher = synchronousPublisher handler
          publishingWriter = publishingEventStoreWriter writer publisher

      _ <- storeEvents publishingWriter (uuidFromInteger 1) NoStream [10, 20, 30 :: Int]

      readIORef ref `shouldReturn` [10, 20, 30]

      -- Verify events are also stored
      events <- getEvents reader (allEvents (uuidFromInteger 1))
      map streamEventEvent events `shouldBe` [10, 20, 30]

    it "should NOT publish events when the write fails" $ do
      tvar <- eventMapTVar
      let (writer, _, _) = makeIOStore tvar

      ref <- newIORef ([] :: [Int])
      let handler = EventHandler $ \(StreamEvent _ _ _ e) -> modifyIORef ref (++ [e])
          publisher = synchronousPublisher handler
          publishingWriter = publishingEventStoreWriter writer publisher

      -- Write should fail due to ExactPosition mismatch
      result <- storeEvents publishingWriter (uuidFromInteger 1) (ExactPosition 5) [10 :: Int]
      result `shouldSatisfy` isLeft'

      readIORef ref `shouldReturn` []

    it "should tag events with correct stream key and version positions" $ do
      tvar <- eventMapTVar
      let (writer, _, _) = makeIOStore tvar

      ref <- newIORef ([] :: [(UUID, EventVersion, Int)])
      let handler = EventHandler $ \(StreamEvent uuid pos _ e) -> modifyIORef ref (++ [(uuid, pos, e)])
          publisher = synchronousPublisher handler
          publishingWriter = publishingEventStoreWriter writer publisher

      let uuid1 = uuidFromInteger 1
      _ <- storeEvents publishingWriter uuid1 NoStream [100, 200 :: Int]

      readIORef ref
        `shouldReturn` [ (uuid1, 0, 100),
                         (uuid1, 1, 200)
                       ]

  describe "breadth-first dispatch (regression test for depth-first bug)" $ do
    it "should deliver all original events before handler-generated events" $ do
      -- This is the critical test: the old EventBus had depth-first dispatch
      -- where if handler A generated new events, those were dispatched to
      -- handler B before handler B saw the rest of the original batch.
      --
      -- With the new EventPublisher, the publish happens AFTER all events
      -- in the batch are written. The handler sees all events in the batch
      -- sequentially. Any events written by a handler are stored but do NOT
      -- trigger re-entrant publishing (because the handler writes to the
      -- raw writer, not the publishing writer).

      tvar <- eventMapTVar
      let (rawWriter, _, _) = makeIOStore tvar

      -- Track the order in which handler B sees events
      orderRef <- newIORef ([] :: [String])

      -- Handler A: when it sees event "trigger", writes new events to the store
      let handlerA = EventHandler $ \(StreamEvent _ _ _ (e :: String)) ->
            when (e == "trigger") $ do
              _ <-
                storeEvents
                  rawWriter
                  (uuidFromInteger 99)
                  AnyPosition
                  ["handler-a-generated"]
              return ()

          -- Handler B: records every event it receives, in order
          handlerB = EventHandler $ \(StreamEvent _ _ _ e) ->
            modifyIORef orderRef (++ [e])

          publisher = synchronousPublisher (handlerA <> handlerB)
          publishingWriter = publishingEventStoreWriter rawWriter publisher

      -- Write two events. "trigger" will cause handler A to write to the store.
      -- Handler B should see BOTH original events ("trigger" and "normal")
      -- sequentially. Handler A's written events go to the raw writer and
      -- are NOT re-published through the handler chain.
      _ <- storeEvents publishingWriter (uuidFromInteger 1) NoStream ["trigger", "normal" :: String]

      observed <- readIORef orderRef

      -- Handler B should see both original events in order
      observed `shouldBe` ["trigger", "normal"]

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' (Right _) = False
