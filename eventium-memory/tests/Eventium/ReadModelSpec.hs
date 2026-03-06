module Eventium.ReadModelSpec (spec) where

import Control.Concurrent.STM
import Eventium
import Eventium.Store.Memory
import Eventium.Testkit
import Test.Hspec

spec :: Spec
spec = do
  describe "rebuildReadModel" $ do
    it "should process all events from the beginning" $ do
      eventTVar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter eventTVar)
          globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader eventTVar)

      -- Write some events
      _ <- writer.storeEvents (uuidFromInteger 1) NoStream [Added 1, Added 2]
      _ <- writer.storeEvents (uuidFromInteger 2) NoStream [Added 10]

      -- Create a simple in-memory read model that sums all Added amounts
      sumRef <- newTVarIO (0 :: Int)
      checkpointRef <- newTVarIO (0 :: SequenceNumber)
      initCount <- newTVarIO (0 :: Int)
      resetCount <- newTVarIO (0 :: Int)

      let rm =
            ReadModel
              { initialize = atomically $ modifyTVar' initCount (+ 1),
                eventHandler = EventHandler $ \globalEvent -> do
                  let innerEvent = globalEvent.payload
                  case innerEvent.payload of
                    Added n -> atomically $ modifyTVar' sumRef (+ n)
                    _ -> return (),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ do
                  writeTVar sumRef 0
                  writeTVar checkpointRef 0
                  modifyTVar' resetCount (+ 1)
              }

      rebuildReadModel globalReader rm

      result <- readTVarIO sumRef
      result `shouldBe` 13

      checkpoint <- readTVarIO checkpointRef
      checkpoint `shouldBe` 3

      inits <- readTVarIO initCount
      inits `shouldBe` 1

      resets <- readTVarIO resetCount
      resets `shouldBe` 1

    it "should reset state before replaying" $ do
      eventTVar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter eventTVar)
          globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader eventTVar)

      _ <- writer.storeEvents (uuidFromInteger 1) NoStream [Added 5]

      sumRef <- newTVarIO (0 :: Int)
      checkpointRef <- newTVarIO (0 :: SequenceNumber)

      let rm =
            ReadModel
              { initialize = return (),
                eventHandler = EventHandler $ \globalEvent -> do
                  let innerEvent = globalEvent.payload
                  case innerEvent.payload of
                    Added n -> atomically $ modifyTVar' sumRef (+ n)
                    _ -> return (),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ do
                  writeTVar sumRef 0
                  writeTVar checkpointRef 0
              }

      -- Rebuild twice — should get same result, not double
      rebuildReadModel globalReader rm
      rebuildReadModel globalReader rm

      result <- readTVarIO sumRef
      result `shouldBe` 5

  describe "combineReadModels" $ do
    it "should fan out events to multiple handlers" $ do
      eventTVar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter eventTVar)
          globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader eventTVar)

      _ <- writer.storeEvents (uuidFromInteger 1) NoStream [Added 3, Added 7]

      sumRef <- newTVarIO (0 :: Int)
      countRef <- newTVarIO (0 :: Int)
      checkpointRef <- newTVarIO (0 :: SequenceNumber)

      let sumRM =
            ReadModel
              { initialize = return (),
                eventHandler = EventHandler $ \globalEvent -> do
                  case globalEvent.payload.payload of
                    Added n -> atomically $ modifyTVar' sumRef (+ n)
                    _ -> return (),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ do
                  writeTVar sumRef 0
                  writeTVar checkpointRef 0
              }

      let countRM =
            ReadModel
              { initialize = return (),
                eventHandler = EventHandler $ \_ ->
                  atomically $ modifyTVar' countRef (+ 1),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ do
                  writeTVar countRef 0
                  writeTVar checkpointRef 0
              }

      let combined = combineReadModels [sumRM, countRM]

      rebuildReadModel globalReader combined

      s <- readTVarIO sumRef
      s `shouldBe` 10

      c <- readTVarIO countRef
      c `shouldBe` 2

    it "should initialize and reset all sub-models" $ do
      initA <- newTVarIO False
      initB <- newTVarIO False
      resetA <- newTVarIO False
      resetB <- newTVarIO False
      checkpointRef <- newTVarIO (0 :: SequenceNumber)

      let mkRM initRef resetRef =
            ReadModel
              { initialize = atomically $ writeTVar initRef True,
                eventHandler = mempty,
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ writeTVar resetRef True
              }

      let combined = combineReadModels [mkRM initA resetA, mkRM initB resetB]
          runInit = combined.initialize
          runReset = combined.reset

      runInit
      iA <- readTVarIO initA
      iB <- readTVarIO initB
      iA `shouldBe` True
      iB `shouldBe` True

      runReset
      rA <- readTVarIO resetA
      rB <- readTVarIO resetB
      rA `shouldBe` True
      rB `shouldBe` True
