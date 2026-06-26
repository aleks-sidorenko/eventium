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

  describe "readModelPublisher (synchronous driver)" $ do
    it "applies the handler and advances the checkpoint during the write" $ do
      eventTVar <- eventMapTVar
      sumRef <- newTVarIO (0 :: Int)
      checkpointRef <- newTVarIO (0 :: SequenceNumber)
      let baseWriter = runEventStoreWriterUsing atomically (tvarEventStoreWriter eventTVar)
          globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader eventTVar)
          rm =
            ReadModel
              { initialize = pure (),
                eventHandler = EventHandler $ \globalEvent ->
                  case globalEvent.payload.payload of
                    Added n -> atomically $ modifyTVar' sumRef (+ n)
                    _ -> return (),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = pure ()
              }
          -- Same ReadModel, driven synchronously in the write path.
          writer = publishingGlobalEventStoreWriter baseWriter (readModelPublisher rm)

      _ <- writer.storeEvents (uuidFromInteger 1) NoStream [Added 1, Added 2]
      _ <- writer.storeEvents (uuidFromInteger 2) NoStream [Added 10]

      -- Handler ran synchronously during the writes.
      readTVarIO sumRef `shouldReturn` 13
      -- Checkpoint advanced in-line to the last assigned global position.
      readTVarIO checkpointRef `shouldReturn` 3
      -- Nothing is left for an async catch-up: the checkpoint is already current.
      newEvents <- globalReader.getEvents (eventsStartingAt () 4)
      length newEvents `shouldBe` 0

  describe "catchUpReadModel" $ do
    it "brings a model current without resetting, and is a no-op when current" $ do
      eventTVar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter eventTVar)
          globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader eventTVar)
      _ <- writer.storeEvents (uuidFromInteger 1) NoStream [Added 1, Added 2]
      _ <- writer.storeEvents (uuidFromInteger 2) NoStream [Added 10]

      sumRef <- newTVarIO (0 :: Int)
      checkpointRef <- newTVarIO (0 :: SequenceNumber)
      resetCount <- newTVarIO (0 :: Int)
      let rm =
            ReadModel
              { initialize = pure (),
                eventHandler = EventHandler $ \globalEvent ->
                  case globalEvent.payload.payload of
                    Added n -> atomically $ modifyTVar' sumRef (+ n)
                    _ -> return (),
                checkpointStore =
                  CheckpointStore
                    { getCheckpoint = readTVarIO checkpointRef,
                      saveCheckpoint = atomically . writeTVar checkpointRef
                    },
                reset = atomically $ modifyTVar' resetCount (+ 1)
              }

      catchUpReadModel globalReader rm
      readTVarIO sumRef `shouldReturn` 13
      readTVarIO checkpointRef `shouldReturn` 3
      -- Did not reset, and a second catch-up applies nothing new.
      catchUpReadModel globalReader rm
      readTVarIO sumRef `shouldReturn` 13
      readTVarIO resetCount `shouldReturn` 0
