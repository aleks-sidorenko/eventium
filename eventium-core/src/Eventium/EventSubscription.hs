{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an event subscription abstraction for push-based event delivery.
--
-- An 'EventSubscription' cleanly separates the delivery mechanism from the
-- consumer (which is just an 'EventHandler').
--
-- This design makes it easy to swap delivery mechanisms (polling, RabbitMQ,
-- Kafka, etc.) without changing the consumer code.
module Eventium.EventSubscription
  ( EventSubscription (..),
    CheckpointStore (..),
    PollingIntervalMillis,
    pollingSubscription,
    runProjectionSubscription,
    RetryConfig (..),
    defaultRetryConfig,
    resilientPollingSubscription,
    resilientRunProjectionSubscription,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import Eventium.EventHandler
import Eventium.Projection
import Eventium.Store.Class

-- | Polling interval in milliseconds.
type PollingIntervalMillis = Int

-- | A pair of actions for reading and writing checkpoint state.
--
-- Groups the get\/set operations for the state a subscription needs to
-- persist between polls (e.g. a sequence number, or a sequence number
-- paired with projection state).
data CheckpointStore m a = CheckpointStore
  { getCheckpoint :: m a,
    saveCheckpoint :: a -> m ()
  }

-- | An 'EventSubscription' delivers events to an 'EventHandler' using
-- some delivery mechanism (polling, message queue, etc.).
--
-- Call 'runSubscription' with an 'EventHandler' to start consuming events.
-- The subscription runs indefinitely (typically in a separate thread).
newtype EventSubscription m event = EventSubscription
  { runSubscription :: EventHandler m event -> m ()
  }

-- | A single poll cycle: read checkpoint, fetch new events, handle them,
-- update checkpoint, delay.
pollOnce ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  CheckpointStore m SequenceNumber ->
  PollingIntervalMillis ->
  EventHandler m (GlobalStreamEvent event) ->
  m ()
pollOnce globalReader checkpoint pollIntervalMs handler = do
  latestSeq <- getCheckpoint checkpoint
  newEvents <- getEvents globalReader (eventsStartingAt () $ latestSeq + 1)
  handleEvents handler newEvents
  case NE.nonEmpty newEvents of
    Nothing -> return ()
    Just ne -> saveCheckpoint checkpoint (streamEventPosition $ NE.last ne)
  delayMillis pollIntervalMs

-- | Create a polling-based 'EventSubscription' that reads from the global
-- event stream at a regular interval.
--
-- The caller is responsible for lifting the store reader into the target
-- monad (e.g. via 'runEventStoreReaderUsing') before passing it in.
--
-- Parameters:
--
-- * @globalReader@ — the global event store reader (already in monad @m@)
-- * @checkpoint@ — checkpoint store for tracking the consumed position
-- * @pollIntervalMs@ — milliseconds to wait between polls
pollingSubscription ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  CheckpointStore m SequenceNumber ->
  PollingIntervalMillis ->
  EventSubscription m (GlobalStreamEvent event)
pollingSubscription globalReader checkpoint pollIntervalMs =
  EventSubscription $ \handler ->
    forever $
      pollOnce globalReader checkpoint pollIntervalMs handler

projectionPollOnce ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  Projection state (VersionedStreamEvent event) ->
  CheckpointStore m (SequenceNumber, state) ->
  PollingIntervalMillis ->
  m ()
projectionPollOnce globalReader proj checkpoint pollIntervalMs = do
  (latestSeq, currentState) <- getCheckpoint checkpoint
  newEvents <- getEvents globalReader (eventsStartingAt () $ latestSeq + 1)
  let (finalSeq, finalState) = foldl' applyGlobalEvent (latestSeq, currentState) newEvents
  saveCheckpoint checkpoint (finalSeq, finalState)
  delayMillis pollIntervalMs
  where
    applyGlobalEvent (_, state) globalEvent =
      let innerEvent = streamEventEvent globalEvent
          newState = projectionEventHandler proj state innerEvent
          newSeq = streamEventPosition globalEvent
       in (newSeq, newState)

-- | Convenience function to run a polling loop that maintains a
-- 'GlobalStreamProjection'. The projection state is updated with each
-- batch of events and the latest sequence number is tracked.
--
-- The 'CheckpointStore' allows the caller to store the
-- @(SequenceNumber, state)@ pair wherever they choose (e.g. TVar, IORef,
-- database).
runProjectionSubscription ::
  (MonadIO m) =>
  GlobalEventStoreReader m event ->
  Projection state (VersionedStreamEvent event) ->
  CheckpointStore m (SequenceNumber, state) ->
  PollingIntervalMillis ->
  m ()
runProjectionSubscription globalReader proj checkpoint pollIntervalMs =
  forever $ projectionPollOnce globalReader proj checkpoint pollIntervalMs

-- | Configuration for retry behavior when a polling subscription
-- encounters an exception.
data RetryConfig = RetryConfig
  { -- | Initial delay before the first retry, in milliseconds.
    retryInitialDelayMs :: !Int,
    -- | Maximum backoff delay in milliseconds (caps exponential growth).
    retryMaxDelayMs :: !Int,
    -- | Multiplier for exponential backoff on consecutive failures.
    retryBackoffMultiplier :: !Double,
    -- | Predicate called on each exception. Return 'True' to retry
    -- (after backoff), 'False' to re-throw and kill the subscription.
    retryOnError :: SomeException -> IO Bool,
    -- | Callback invoked on each caught exception. Receives the exception
    -- and the consecutive error count (1 on first failure). Use for logging.
    retryOnErrorCallback :: SomeException -> Int -> IO ()
  }

-- | Sensible defaults: 1 second initial delay, 30 second cap, 2x backoff,
-- always retry, no-op callback.
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { retryInitialDelayMs = 1000,
      retryMaxDelayMs = 30000,
      retryBackoffMultiplier = 2.0,
      retryOnError = const (return True),
      retryOnErrorCallback = \_ _ -> return ()
    }

-- | Like 'pollingSubscription' but with automatic error recovery.
--
-- When an exception occurs during a poll cycle, the exception is caught
-- and the subscription retries after an exponentially increasing backoff
-- delay. On a successful poll, the backoff resets to zero.
--
-- The @unlift@ parameter converts @m@ actions to @IO@, needed to catch
-- exceptions. This follows the same pattern as 'runEventStoreReaderUsing'.
--
-- @
-- -- With m ~ IO:
-- resilientPollingSubscription id globalReader checkpoint 500 defaultRetryConfig
--
-- -- With m ~ SqlPersistT IO:
-- resilientPollingSubscription (flip runSqlPool pool) globalReader checkpoint 500 defaultRetryConfig
-- @
resilientPollingSubscription ::
  (MonadIO m) =>
  (forall a. m a -> IO a) ->
  GlobalEventStoreReader m event ->
  CheckpointStore m SequenceNumber ->
  PollingIntervalMillis ->
  RetryConfig ->
  EventSubscription m (GlobalStreamEvent event)
resilientPollingSubscription unlift globalReader checkpoint pollIntervalMs retryConfig =
  EventSubscription $ \handler -> loop handler 0
  where
    loop handler consecutiveErrors = do
      result <- liftIO $ try $ unlift $ pollOnce globalReader checkpoint pollIntervalMs handler
      case result of
        Right () -> loop handler 0
        Left (ex :: SomeException) -> do
          shouldRetry <- liftIO $ retryOnError retryConfig ex
          if shouldRetry
            then do
              let newCount = consecutiveErrors + 1
              liftIO $ retryOnErrorCallback retryConfig ex newCount
              liftIO $ threadDelay (backoffMicros retryConfig newCount)
              loop handler newCount
            else liftIO $ throwIO ex

-- | Like 'runProjectionSubscription' but with automatic error recovery.
-- See 'resilientPollingSubscription' for details on the retry behavior.
resilientRunProjectionSubscription ::
  (MonadIO m) =>
  (forall a. m a -> IO a) ->
  GlobalEventStoreReader m event ->
  Projection state (VersionedStreamEvent event) ->
  CheckpointStore m (SequenceNumber, state) ->
  PollingIntervalMillis ->
  RetryConfig ->
  m ()
resilientRunProjectionSubscription unlift globalReader proj checkpoint pollIntervalMs retryConfig =
  loop 0
  where
    loop consecutiveErrors = do
      result <- liftIO $ try $ unlift $ projectionPollOnce globalReader proj checkpoint pollIntervalMs
      case result of
        Right () -> loop 0
        Left (ex :: SomeException) -> do
          shouldRetry <- liftIO $ retryOnError retryConfig ex
          if shouldRetry
            then do
              let newCount = consecutiveErrors + 1
              liftIO $ retryOnErrorCallback retryConfig ex newCount
              liftIO $ threadDelay (backoffMicros retryConfig newCount)
              loop newCount
            else liftIO $ throwIO ex

backoffMicros :: RetryConfig -> Int -> Int
backoffMicros RetryConfig {..} count =
  let delayMs =
        min retryMaxDelayMs $
          round (fromIntegral retryInitialDelayMs * retryBackoffMultiplier ^^ max 0 (count - 1))
   in delayMs * 1000

-- | Sleep for the given number of milliseconds.
delayMillis :: (MonadIO m) => PollingIntervalMillis -> m ()
delayMillis ms = liftIO $ threadDelay (ms * 1000)
