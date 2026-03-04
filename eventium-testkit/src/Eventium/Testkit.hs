{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Common test functionality
module Eventium.Testkit
  ( Counter (..),
    CounterProjection,
    counterProjection,
    CounterCommandHandler,
    CounterCommandError (..),
    counterCommandHandler,
    CounterEvent (..),
    CounterCommand (..),
    allCommandHandlerStates,
    EventStoreRunner (..),
    GlobalStreamEventStoreRunner (..),
    eventStoreSpec,
    globalStreamEventStoreSpec,
    VersionedProjectionCacheRunner (..),
    versionedProjectionCacheSpec,
    GlobalStreamProjectionCacheRunner (..),
    globalStreamProjectionCacheSpec,
    Text,
    module X,
  )
where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X
import Data.Aeson
import Data.Aeson.TH
import Data.List (scanl')
import Data.Text (Text)
import Eventium
import Test.Hspec

-- | Example Projection/CommandHandler
newtype Counter = Counter {unCounter :: Int}
  deriving (Eq, Show, FromJSON, ToJSON)

data CounterEvent
  = Added
      { amount :: Int
      }
  | CounterFailedOutOfBounds
  deriving (Eq, Show)

type CounterProjection = Projection Counter CounterEvent

counterProjection :: CounterProjection
counterProjection =
  Projection
    (Counter 0)
    ( \(Counter k) event -> case event of
        Added x -> Counter (k + x)
        CounterFailedOutOfBounds -> Counter k
    )

counterGlobalProjection :: Projection Counter (VersionedStreamEvent CounterEvent)
counterGlobalProjection =
  Projection
    (Counter 0)
    ( \(Counter k) streamEvent -> case streamEvent.payload of
        Added x -> Counter (k + x)
        CounterFailedOutOfBounds -> Counter k
    )

data CounterCommand
  = Increment
      { amount :: Int
      }
  | Decrement
      { amount :: Int
      }
  deriving (Eq, Show)

-- | Error type for counter command rejections.
data CounterCommandError = CounterOutOfBounds
  deriving (Eq, Show)

type CounterCommandHandler = CommandHandler Counter CounterEvent CounterCommand CounterCommandError

counterCommandHandler :: CounterCommandHandler
counterCommandHandler = CommandHandler counterCommand counterProjection

counterCommand :: Counter -> CounterCommand -> Either CounterCommandError [CounterEvent]
counterCommand (Counter k) (Increment n) =
  if k + n <= 100
    then Right [Added n]
    else Left CounterOutOfBounds
counterCommand (Counter k) (Decrement n) =
  if k - n >= 0
    then Right [Added (-n)]
    else Left CounterOutOfBounds

deriveJSON defaultOptions ''CounterEvent
deriveJSON defaultOptions ''CounterCommand

-- | Given a list of commands, produce all of the states the command handler's
-- projection sees. This is useful for unit testing a 'CommandHandler' by
-- verifying the intermediate states produced by a sequence of commands.
--
-- Commands that are rejected ('Left') leave the state unchanged.
-- The first element of the returned list is the projection's seed state.
--
-- This is the canonical location for this utility. It was moved here from
-- @eventium-core@ so that production code does not depend on test helpers.
--
-- @
-- allCommandHandlerStates handler [cmd1, cmd2, cmd3]
--   == [seed, stateAfterCmd1, stateAfterCmd2, stateAfterCmd3]
-- @
allCommandHandlerStates ::
  CommandHandler state event command err ->
  [command] ->
  [state]
allCommandHandlerStates (CommandHandler commandHandler (Projection seed' eventHandler)) =
  scanl' go seed'
  where
    go st command = case commandHandler st command of
      Left _ -> st
      Right events -> foldl' eventHandler st events

-- Test harness for stores

newtype EventStoreRunner m
  = EventStoreRunner (forall a. (VersionedEventStoreWriter m CounterEvent -> VersionedEventStoreReader m CounterEvent -> m a) -> IO a)

eventStoreSpec ::
  (Monad m) =>
  EventStoreRunner m ->
  Spec
eventStoreSpec (EventStoreRunner withStore) = do
  let withStoreExampleEvents action = withStore $ \writer reader -> do
        _ <- insertExampleEvents writer
        action writer reader

  context "when a few events are inserted" $ do
    let sampleEvents = [Added 1, Added 4, Added (-3), Added 5]
        withStore' action = withStore $ \writer reader -> do
          _ <- writer.storeEvents nil NoStream sampleEvents
          action writer reader

    it "should return events" $ do
      events' <- withStore' $ \_ reader -> reader.getEvents (allEvents nil)
      ((.payload) <$> events') `shouldBe` sampleEvents

    it "should return correct event versions" $ do
      events <- withStore' $ \_ reader -> reader.getEvents (allEvents nil)
      ((.position) <$> events) `shouldBe` [0, 1, 2, 3]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore' $ \_ reader ->
        (,,,)
          <$> reader.getEvents (eventsUntil nil 1)
          <*> reader.getEvents (eventsStartingAtUntil nil 1 2)
          <*> reader.getEvents (eventsStartingAt nil 2)
          <*> reader.getEvents (eventsStartingAtTakeLimit nil 0 2)
      ((.payload) <$> firstEvents) `shouldBe` take 2 sampleEvents
      ((.payload) <$> middleEvents) `shouldBe` take 2 (drop 1 sampleEvents)
      ((.payload) <$> laterEvents) `shouldBe` drop 2 sampleEvents
      ((.payload) <$> maxEvents) `shouldBe` take 2 sampleEvents

    it "should return the latest projection" $ do
      proj <- withStore' $ \_ reader ->
        getLatestStreamProjection reader (versionedStreamProjection nil counterProjection)
      proj.state `shouldBe` Counter 7
      proj.position `shouldBe` 3
      proj.key `shouldBe` nil

    it "should return the latest projection with some starting StreamProjection" $ do
      proj <- withStore' $ \_ reader -> do
        initialEvents <- reader.getEvents (eventsUntil nil 1)
        let initialProjection = latestProjection counterProjection ((.payload) <$> initialEvents)
        getLatestStreamProjection reader (StreamProjection nil 1 counterProjection initialProjection)
      proj.state `shouldBe` Counter 7
      proj.position `shouldBe` 3
      proj.key `shouldBe` nil

  context "when events from multiple UUIDs are inserted" $ do
    it "should have the correct events for each stream" $ do
      (events1, events2) <- withStoreExampleEvents $ \_ reader ->
        (,) <$> reader.getEvents (allEvents uuid1) <*> reader.getEvents (allEvents uuid2)
      ((.payload) <$> events1) `shouldBe` Added <$> [1, 4]
      ((.payload) <$> events2) `shouldBe` Added <$> [2, 3, 5]
      ((.key) <$> events1) `shouldBe` [uuid1, uuid1]
      ((.key) <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      ((.position) <$> events1) `shouldBe` [0, 1]
      ((.position) <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (events1, events2) <- withStoreExampleEvents $ \_ reader ->
        (,)
          <$> reader.getEvents (allEvents uuid1)
          <*> reader.getEvents (allEvents uuid2)
      (.payload) <$> events1 `shouldBe` [Added 1, Added 4]
      (.payload) <$> events2 `shouldBe` [Added 2, Added 3, Added 5]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStoreExampleEvents $ \_ reader ->
        (,,,)
          <$> reader.getEvents (eventsUntil uuid1 1)
          <*> reader.getEvents (eventsStartingAtUntil uuid2 1 2)
          <*> reader.getEvents (eventsStartingAt uuid2 2)
          <*> reader.getEvents (eventsStartingAtTakeLimit uuid1 1 1)
      ((.payload) <$> firstEvents) `shouldBe` [Added 1, Added 4]
      ((.payload) <$> middleEvents) `shouldBe` [Added 3, Added 5]
      ((.payload) <$> laterEvents) `shouldBe` [Added 5]
      ((.payload) <$> maxEvents) `shouldBe` [Added 4]

    it "should produce the correct projections" $ do
      (proj1, proj2) <- withStoreExampleEvents $ \_ reader ->
        (,)
          <$> getLatestStreamProjection reader (versionedStreamProjection uuid1 counterProjection)
          <*> getLatestStreamProjection reader (versionedStreamProjection uuid2 counterProjection)
      (proj1.state, proj1.position) `shouldBe` (Counter 5, 1)
      (proj2.state, proj2.position) `shouldBe` (Counter 10, 2)

  describe "can handle event storage errors" $ do
    it "rejects some writes when event store isn't created" $ do
      (err1, err2) <- withStore $ \writer _ ->
        (,)
          <$> writer.storeEvents nil StreamExists [Added 1]
          <*> writer.storeEvents nil (ExactPosition 0) [Added 1]
      err1 `shouldBe` Left (EventStreamNotAtExpectedVersion (-1))
      err2 `shouldBe` Left (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      withStore (\writer _ -> writer.storeEvents nil NoStream [Added 1]) `shouldReturn` Right 0

    it "should reject storing events sometimes with a stream" $ do
      (err1, err2, err3) <- withStore $ \writer _ ->
        (,,)
          <$> writer.storeEvents nil NoStream [Added 1]
          <*> writer.storeEvents nil NoStream [Added 1]
          <*> writer.storeEvents nil (ExactPosition 1) [Added 1]
      err1 `shouldBe` Right 0
      err2 `shouldBe` Left (EventStreamNotAtExpectedVersion 0)
      err3 `shouldBe` Left (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      errors <- withStore $ \writer _ ->
        sequence
          [ writer.storeEvents nil NoStream [Added 1],
            writer.storeEvents nil AnyPosition [Added 1],
            writer.storeEvents nil (ExactPosition 1) [Added 1],
            writer.storeEvents nil StreamExists [Added 1]
          ]
      errors `shouldBe` [Right 0, Right 1, Right 2, Right 3]

newtype GlobalStreamEventStoreRunner m
  = GlobalStreamEventStoreRunner
      (forall a. (VersionedEventStoreWriter m CounterEvent -> GlobalEventStoreReader m CounterEvent -> m a) -> IO a)

globalStreamEventStoreSpec ::
  (Monad m) =>
  GlobalStreamEventStoreRunner m ->
  Spec
globalStreamEventStoreSpec (GlobalStreamEventStoreRunner withStore) = do
  context "when the event store is empty" $ do
    it "shouldn't have any events" $ do
      events <- withStore (\_ globalReader -> globalReader.getEvents (allEvents ()))
      length events `shouldBe` 0

  context "when events from multiple UUIDs are inserted" $ do
    it "should have the correct events in global order" $ do
      events <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        globalReader.getEvents (allEvents ())
      ((\e -> e.payload.payload) <$> events) `shouldBe` Added <$> [1 .. 5]
      ((\e -> e.payload.key) <$> events) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      ((\e -> e.payload.position) <$> events) `shouldBe` [0, 0, 1, 1, 2]
      ((.position) <$> events) `shouldBe` [1 .. 5]

    it "should work with global projections" $ do
      (proj1, proj2) <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        p1 <- getLatestStreamProjection globalReader (globalStreamProjection counterGlobalProjection)
        _ <- writer.storeEvents uuid1 AnyPosition [Added 10, Added 20]
        p2 <- getLatestStreamProjection globalReader p1
        return (p1, p2)

      proj1.position `shouldBe` 5
      proj2.position `shouldBe` 7

    it "should handle queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        (,,,)
          <$> globalReader.getEvents (eventsUntil () 2)
          <*> globalReader.getEvents (eventsStartingAtUntil () 2 3)
          <*> globalReader.getEvents (eventsStartingAt () 3)
          <*> globalReader.getEvents (eventsStartingAtTakeLimit () 2 3)

      ((\e -> e.payload.payload) <$> firstEvents) `shouldBe` Added <$> [1 .. 2]
      ((.position) <$> firstEvents) `shouldBe` [1 .. 2]
      ((\e -> e.payload.payload) <$> middleEvents) `shouldBe` Added <$> [2 .. 3]
      ((.position) <$> middleEvents) `shouldBe` [2 .. 3]
      ((\e -> e.payload.payload) <$> laterEvents) `shouldBe` Added <$> [3 .. 5]
      ((.position) <$> laterEvents) `shouldBe` [3 .. 5]
      ((\e -> e.payload.payload) <$> maxEvents) `shouldBe` Added <$> [2 .. 4]
      ((.position) <$> maxEvents) `shouldBe` [2 .. 4]

insertExampleEvents ::
  (Monad m) =>
  VersionedEventStoreWriter m CounterEvent ->
  m ()
insertExampleEvents store = do
  void $ store.storeEvents uuid1 NoStream [Added 1]
  void $ store.storeEvents uuid2 NoStream [Added 2, Added 3]
  void $ store.storeEvents uuid1 (ExactPosition 0) [Added 4]
  void $ store.storeEvents uuid2 (ExactPosition 1) [Added 5]

uuid1 :: UUID
uuid1 = uuidFromInteger 1

uuid2 :: UUID
uuid2 = uuidFromInteger 2

newtype VersionedProjectionCacheRunner m
  = VersionedProjectionCacheRunner
      ( forall a.
        ( VersionedEventStoreWriter m CounterEvent ->
          VersionedEventStoreReader m CounterEvent ->
          VersionedProjectionCache Counter m ->
          m a
        ) ->
        IO a
      )

versionedProjectionCacheSpec ::
  (Monad m) =>
  VersionedProjectionCacheRunner m ->
  Spec
versionedProjectionCacheSpec (VersionedProjectionCacheRunner withStoreAndCache) = do
  context "when the store is empty" $ do
    it "should be able to store and load simple projections" $ do
      snapshot <- withStoreAndCache $ \_ _ cache -> do
        cache.storeProjectionSnapshot nil 4 (Counter 100)
        cache.loadProjectionSnapshot nil
      snapshot `shouldBe` Just (4, Counter 100)

  context "when the store has some events in one stream" $ do
    it "should load from a stream of events" $ do
      snapshot <- withStoreAndCache $ \writer reader cache -> do
        _ <- writer.storeEvents nil AnyPosition [Added 1, Added 2]
        getLatestVersionedProjectionWithCache reader cache (versionedStreamProjection nil counterProjection)
      snapshot.position `shouldBe` 1
      snapshot.state `shouldBe` Counter 3

    it "should work with updateProjectionCache" $ do
      snapshot <- withStoreAndCache $ \writer reader cache -> do
        _ <- writer.storeEvents nil AnyPosition [Added 1, Added 2, Added 3]
        updateProjectionCache reader cache (versionedStreamProjection nil counterProjection)
        getLatestVersionedProjectionWithCache reader cache (versionedStreamProjection nil counterProjection)
      snapshot.key `shouldBe` nil
      snapshot.position `shouldBe` 2
      snapshot.state `shouldBe` Counter 6

newtype GlobalStreamProjectionCacheRunner m
  = GlobalStreamProjectionCacheRunner
      ( forall a.
        ( VersionedEventStoreWriter m CounterEvent ->
          GlobalEventStoreReader m CounterEvent ->
          GlobalStreamProjectionCache Text Counter m ->
          m a
        ) ->
        IO a
      )

globalStreamProjectionCacheSpec ::
  (Monad m) =>
  GlobalStreamProjectionCacheRunner m ->
  Spec
globalStreamProjectionCacheSpec (GlobalStreamProjectionCacheRunner withStoreAndCache) = do
  context "when the store is empty" $ do
    it "should be able to store and load simple projections" $ do
      snapshot <- withStoreAndCache $ \_ _ cache -> do
        cache.storeProjectionSnapshot "key" 4 (Counter 100)
        cache.loadProjectionSnapshot "key"
      snapshot `shouldBe` Just (4, Counter 100)

  context "when the store has some events in one stream" $ do
    it "should load from a global stream of events" $ do
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        _ <- writer.storeEvents nil AnyPosition [Added 1, Added 2]
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      snapshot.position `shouldBe` 2
      snapshot.state `shouldBe` Counter 3

    it "should work with updateGlobalProjectionCache" $ do
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        _ <- writer.storeEvents nil AnyPosition [Added 1, Added 2, Added 3]
        updateGlobalProjectionCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      snapshot.position `shouldBe` 3
      snapshot.state `shouldBe` Counter 6

  context "when events from multiple UUIDs are inserted" $ do
    it "should have the correct cached projection value" $ do
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        insertExampleEvents writer
        updateGlobalProjectionCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      snapshot.position `shouldBe` 5
      snapshot.state `shouldBe` Counter 15
