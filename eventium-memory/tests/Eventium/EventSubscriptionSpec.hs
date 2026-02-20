module Eventium.EventSubscriptionSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Data.IORef
import Eventium.EventHandler
import Eventium.EventSubscription
import Eventium.Store.Class
import Eventium.Store.Memory
import Eventium.UUID
import Test.Hspec

-- Helper to create IO-compatible stores from a TVar
makeIOStores :: TVar (EventMap event) -> (VersionedEventStoreWriter IO event, GlobalEventStoreReader IO event)
makeIOStores tvar =
  ( runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar),
    runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)
  )

spec :: Spec
spec = do
  describe "pollingSubscription" $ do
    it "should deliver events from the global store to a handler" $ do
      tvar <- eventMapTVar
      let (writer, globalReader) = makeIOStores tvar

      -- Write some events
      _ <- storeEvents writer (uuidFromInteger 1) NoStream [10 :: Int, 20]
      _ <- storeEvents writer (uuidFromInteger 2) NoStream [30]

      -- Set up checkpoint tracking
      checkpointRef <- newIORef (0 :: SequenceNumber)
      let getCheckpoint = readIORef checkpointRef
          saveCheckpoint = writeIORef checkpointRef

      -- Set up the subscription with very short poll interval
      let sub = pollingSubscription globalReader (CheckpointStore getCheckpoint saveCheckpoint) 50

      -- Collect delivered events
      deliveredRef <- newIORef ([] :: [Int])
      let handler = EventHandler $ \globalEvent ->
            modifyIORef deliveredRef (++ [streamEventEvent $ streamEventEvent globalEvent])

      -- Run subscription in background, give it time to poll once
      tid <- forkIO $ runSubscription sub handler
      threadDelay 200000 -- 200ms
      killThread tid

      delivered <- readIORef deliveredRef
      delivered `shouldBe` [10, 20, 30]

      -- Checkpoint should be updated
      checkpoint <- readIORef checkpointRef
      checkpoint `shouldBe` 3

    it "should not deliver already-consumed events" $ do
      tvar <- eventMapTVar
      let (writer, globalReader) = makeIOStores tvar

      -- Write some initial events
      _ <- storeEvents writer (uuidFromInteger 1) NoStream [10 :: Int, 20]

      -- Start with checkpoint at 2 (already consumed)
      checkpointRef <- newIORef (2 :: SequenceNumber)
      let getCheckpoint = readIORef checkpointRef
          saveCheckpoint = writeIORef checkpointRef

      let sub = pollingSubscription globalReader (CheckpointStore getCheckpoint saveCheckpoint) 50

      deliveredRef <- newIORef ([] :: [Int])
      let handler = EventHandler $ \globalEvent ->
            modifyIORef deliveredRef (++ [streamEventEvent $ streamEventEvent globalEvent])

      -- Write more events
      _ <- storeEvents writer (uuidFromInteger 2) NoStream [30]

      tid <- forkIO $ runSubscription sub handler
      threadDelay 200000
      killThread tid

      -- Should only see the new event
      delivered <- readIORef deliveredRef
      delivered `shouldBe` [30]
