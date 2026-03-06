module Eventium.ResilientSubscriptionSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Eventium
import Eventium.CheckpointStore.Memory
import Eventium.Store.Memory
import Test.Hspec

spec :: Spec
spec = describe "resilientPollingSubscription" $ do
  it "recovers from a transient error and delivers events" $ do
    tvar <- eventMapTVar
    let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar)
        globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)
        uuid = uuidFromInteger 1
    _ <- writer.storeEvents uuid NoStream [10 :: Int, 20]

    failRef <- newIORef True
    let wrappedReader =
          EventStoreReader $ \range -> do
            shouldFail <- liftIO $ readIORef failRef
            if shouldFail
              then do
                liftIO $ writeIORef failRef False
                liftIO $ throwIO (userError "transient DB error" :: IOException)
              else globalReader.getEvents range

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = ioRefCheckpointStore checkpointRef

    deliveredRef <- newIORef ([] :: [Int])
    let handler = EventHandler $ \ge ->
          modifyIORef deliveredRef (++ [ge.payload.payload])

    callbackRef <- newIORef (0 :: Int)
    let config =
          defaultRetryConfig
            { initialDelayMs = 10,
              maxDelayMs = 100,
              onErrorCallback = \_ n -> writeIORef callbackRef n
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
    tid <- forkIO $ sub.runSubscription handler
    threadDelay 500000
    killThread tid

    delivered <- readIORef deliveredRef
    delivered `shouldBe` [10, 20]
    callbackCount <- readIORef callbackRef
    callbackCount `shouldBe` 1

  it "re-throws when retryOnError returns False" $ do
    let wrappedReader =
          EventStoreReader $ \_ ->
            liftIO $ throwIO (userError "fatal error" :: IOException)

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = ioRefCheckpointStore checkpointRef

    let config =
          defaultRetryConfig
            { onError = const (return False)
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
        handler = EventHandler $ \_ -> return ()

    sub.runSubscription handler `shouldThrow` anyIOException

  it "invokes retryOnErrorCallback with correct consecutive count" $ do
    tvar <- eventMapTVar

    callCountRef <- newIORef (0 :: Int)
    let fallbackReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)
        wrappedReader =
          EventStoreReader $ \_ -> do
            n <- liftIO $ readIORef callCountRef
            if n < 3
              then do
                liftIO $ modifyIORef callCountRef (+ 1)
                liftIO $ throwIO (userError "error" :: IOException)
              else
                fallbackReader.getEvents (allEvents ())

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = ioRefCheckpointStore checkpointRef

    callbackCounts <- newIORef ([] :: [Int])
    let config =
          defaultRetryConfig
            { initialDelayMs = 10,
              maxDelayMs = 100,
              onErrorCallback = \_ n -> modifyIORef callbackCounts (++ [n])
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
        handler = EventHandler $ \_ -> return ()

    tid <- forkIO $ sub.runSubscription handler
    threadDelay 500000
    killThread tid

    counts <- readIORef callbackCounts
    counts `shouldBe` [1, 2, 3]
