module Eventium.ResilientSubscriptionSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Eventium
import Eventium.Store.Memory
import Test.Hspec

spec :: Spec
spec = describe "resilientPollingSubscription" $ do
  it "recovers from a transient error and delivers events" $ do
    tvar <- eventMapTVar
    let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar)
        globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)
        uuid = uuidFromInteger 1
    _ <- storeEvents writer uuid NoStream [10 :: Int, 20]

    failRef <- newIORef True
    let wrappedReader =
          EventStoreReader $ \range -> do
            shouldFail <- liftIO $ readIORef failRef
            if shouldFail
              then do
                liftIO $ writeIORef failRef False
                liftIO $ throwIO (userError "transient DB error" :: IOException)
              else getEvents globalReader range

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = CheckpointStore (readIORef checkpointRef) (writeIORef checkpointRef)

    deliveredRef <- newIORef ([] :: [Int])
    let handler = EventHandler $ \ge ->
          modifyIORef deliveredRef (++ [streamEventEvent $ streamEventEvent ge])

    callbackRef <- newIORef (0 :: Int)
    let config =
          defaultRetryConfig
            { retryInitialDelayMs = 10,
              retryMaxDelayMs = 100,
              retryOnErrorCallback = \_ n -> writeIORef callbackRef n
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
    tid <- forkIO $ runSubscription sub handler
    threadDelay 500000
    killThread tid

    delivered <- readIORef deliveredRef
    delivered `shouldBe` [10, 20]
    callbackCount <- readIORef callbackRef
    callbackCount `shouldBe` 1

  it "re-throws when retryOnError returns False" $ do
    tvar <- eventMapTVar
    let globalReader = runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)

    let wrappedReader =
          EventStoreReader $ \_ ->
            liftIO $ throwIO (userError "fatal error" :: IOException)

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = CheckpointStore (readIORef checkpointRef) (writeIORef checkpointRef)

    let config =
          defaultRetryConfig
            { retryOnError = const (return False)
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
        handler = EventHandler $ \_ -> return ()

    runSubscription sub handler `shouldThrow` anyIOException

  it "invokes retryOnErrorCallback with correct consecutive count" $ do
    tvar <- eventMapTVar

    callCountRef <- newIORef (0 :: Int)
    let wrappedReader =
          EventStoreReader $ \_ -> do
            n <- liftIO $ readIORef callCountRef
            if n < 3
              then do
                liftIO $ modifyIORef callCountRef (+ 1)
                liftIO $ throwIO (userError "error" :: IOException)
              else
                getEvents (runEventStoreReaderUsing atomically (tvarGlobalEventStoreReader tvar)) (allEvents ())

    checkpointRef <- newIORef (0 :: SequenceNumber)
    let checkpoint = CheckpointStore (readIORef checkpointRef) (writeIORef checkpointRef)

    callbackCounts <- newIORef ([] :: [Int])
    let config =
          defaultRetryConfig
            { retryInitialDelayMs = 10,
              retryMaxDelayMs = 100,
              retryOnErrorCallback = \_ n -> modifyIORef callbackCounts (++ [n])
            }

    let sub = resilientPollingSubscription id wrappedReader checkpoint 50 config
        handler = EventHandler $ \_ -> return ()

    tid <- forkIO $ runSubscription sub handler
    threadDelay 500000
    killThread tid

    counts <- readIORef callbackCounts
    counts `shouldBe` [1, 2, 3]
