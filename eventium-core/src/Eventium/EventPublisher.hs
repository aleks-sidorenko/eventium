{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Defines an event publisher abstraction that decouples event notification
-- from event storage.
--
-- An 'EventPublisher' is responsible for distributing newly stored events to
-- interested consumers using breadth-first dispatch.
--
-- Use 'publishingEventStoreWriter' to wrap an 'EventStoreWriter' so that
-- events are automatically published after a successful write.
module Eventium.EventPublisher
  ( -- * Per-stream (versioned) publishing
    EventPublisher (..),
    synchronousPublisher,
    publishingEventStoreWriter,
    publishingTaggedCodecEventStoreWriter,

    -- * Global publishing (real 'SequenceNumber's)
    GlobalEventPublisher (..),
    synchronousGlobalPublisher,
    globalToVersionedHandler,
    publishingGlobalEventStoreWriter,
    publishingGlobalTaggedCodecEventStoreWriter,
  )
where

import Control.Exception (throw)
import Data.Functor.Contravariant (contramap)
import Eventium.Codec (Codec (..), DecodeError (..))
import Eventium.EventHandler
import Eventium.Store.Class
import Eventium.UUID

-- | An 'EventPublisher' pushes versioned stream events to consumers after
-- they have been successfully written to the event store.
newtype EventPublisher m event = EventPublisher
  { publishEvents :: UUID -> [VersionedStreamEvent event] -> m ()
  }

-- | Create an 'EventPublisher' that synchronously delivers events to an
-- 'EventHandler'. Each event in the batch is delivered to the handler in
-- order.
synchronousPublisher ::
  (Monad m) =>
  EventHandler m (VersionedStreamEvent event) ->
  EventPublisher m event
synchronousPublisher handler = EventPublisher $ \_ events ->
  handleEvents handler events

-- | Wrap a 'VersionedEventStoreWriter' so that after a successful write,
-- events are published via the given 'EventPublisher'.
--
-- If the write fails (e.g. due to an optimistic concurrency conflict),
-- no events are published.
--
-- The events are tagged with their stream key and version positions starting
-- from the version returned by the write.
publishingEventStoreWriter ::
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  EventPublisher m event ->
  VersionedEventStoreWriter m event
publishingEventStoreWriter (EventStoreWriter write) (EventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos events -> do
    result <- write uuid expectedPos events
    case result of
      Left err -> return $ Left err
      Right wr -> do
        let versionedEvents = zipWith (\(v, _) e -> StreamEvent uuid v (emptyMetadata "") e) wr events
        publish uuid versionedEvents
        return $ Right wr

-- | Like 'publishingEventStoreWriter' but for writers that accept
-- @TaggedEvent encoded@. Each tagged event's payload is decoded through the
-- supplied 'Codec' before publishing — handlers receive domain events while
-- the writer stores serialized payloads. Metadata from each 'TaggedEvent' is
-- preserved in the 'StreamEvent' wrappers passed to the publisher.
--
-- Throws 'DecodeError' if any event fails to decode.
publishingTaggedCodecEventStoreWriter ::
  (Monad m) =>
  Codec event encoded ->
  VersionedEventStoreWriter m (TaggedEvent encoded) ->
  EventPublisher m event ->
  VersionedEventStoreWriter m (TaggedEvent encoded)
publishingTaggedCodecEventStoreWriter codec (EventStoreWriter write) (EventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos taggedEvents -> do
    result <- write uuid expectedPos taggedEvents
    case result of
      Left err -> return $ Left err
      Right wr -> do
        let versionedEvents =
              zipWith
                ( \(v, _) (TaggedEvent meta enc) ->
                    let event = case codec.decode enc of
                          Just e -> e
                          Nothing -> throw $ DecodeError "publishingTaggedCodecEventStoreWriter" "Failed to decode tagged event payload"
                     in StreamEvent uuid v meta event
                )
                wr
                taggedEvents
        publish uuid versionedEvents
        return $ Right wr

-- -----------------------------------------------------------------------------
-- Global publishing
-- -----------------------------------------------------------------------------

-- | A 'GlobalEventPublisher' pushes 'GlobalStreamEvent's — events carrying their
-- real global 'SequenceNumber' — to consumers after a successful write.
--
-- Unlike 'EventPublisher' (which only knows per-stream versions), this uses the
-- 'WriteResult' to deliver the true global positions, so a synchronous
-- subscriber (e.g. a 'Eventium.ReadModel.ReadModel' driven in the write
-- transaction) sees the same positions the global reader would.
newtype GlobalEventPublisher m event = GlobalEventPublisher
  { publishGlobalEvents :: [GlobalStreamEvent event] -> m ()
  }

-- | Fan out to both publishers, in order.
instance (Applicative m) => Semigroup (GlobalEventPublisher m event) where
  GlobalEventPublisher p1 <> GlobalEventPublisher p2 =
    GlobalEventPublisher $ \events -> p1 events *> p2 events

instance (Applicative m) => Monoid (GlobalEventPublisher m event) where
  mempty = GlobalEventPublisher $ \_ -> pure ()

-- | A 'GlobalEventPublisher' that synchronously delivers each global event to an
-- 'EventHandler'.
synchronousGlobalPublisher ::
  (Monad m) =>
  EventHandler m (GlobalStreamEvent event) ->
  GlobalEventPublisher m event
synchronousGlobalPublisher handler = GlobalEventPublisher (handleEvents handler)

-- | Lift a per-stream ('VersionedStreamEvent') handler to consume
-- 'GlobalStreamEvent's, by projecting to the inner versioned event. Lets
-- existing versioned consumers (process managers, loggers) compose into a
-- 'GlobalEventPublisher'.
globalToVersionedHandler ::
  EventHandler m (VersionedStreamEvent event) ->
  EventHandler m (GlobalStreamEvent event)
globalToVersionedHandler = contramap (.payload)

-- | Build a 'GlobalStreamEvent' for each written event from the 'WriteResult'
-- (its per-stream version + assigned global position) and its payload.
toGlobalStreamEvents ::
  UUID ->
  WriteResult ->
  (a -> (EventMetadata, event)) ->
  [a] ->
  [GlobalStreamEvent event]
toGlobalStreamEvents uuid wr unpack =
  zipWith
    ( \(v, gseq) a ->
        let (meta, event) = unpack a
         in StreamEvent () gseq meta (StreamEvent uuid v meta event)
    )
    wr

-- | Like 'publishingEventStoreWriter' but publishes 'GlobalStreamEvent's with
-- real global positions via a 'GlobalEventPublisher'.
publishingGlobalEventStoreWriter ::
  (Monad m) =>
  VersionedEventStoreWriter m event ->
  GlobalEventPublisher m event ->
  VersionedEventStoreWriter m event
publishingGlobalEventStoreWriter (EventStoreWriter write) (GlobalEventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos events -> do
    result <- write uuid expectedPos events
    case result of
      Left err -> return $ Left err
      Right wr -> do
        publish (toGlobalStreamEvents uuid wr (emptyMetadata "",) events)
        return $ Right wr

-- | Like 'publishingTaggedCodecEventStoreWriter' but publishes
-- 'GlobalStreamEvent's with real global positions. Each tagged payload is
-- decoded through the 'Codec'; the tagged metadata is preserved on both the
-- inner versioned event and the outer global event (mirroring the global
-- reader).
--
-- Throws 'DecodeError' if any event fails to decode.
publishingGlobalTaggedCodecEventStoreWriter ::
  (Monad m) =>
  Codec event encoded ->
  VersionedEventStoreWriter m (TaggedEvent encoded) ->
  GlobalEventPublisher m event ->
  VersionedEventStoreWriter m (TaggedEvent encoded)
publishingGlobalTaggedCodecEventStoreWriter codec (EventStoreWriter write) (GlobalEventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos taggedEvents -> do
    result <- write uuid expectedPos taggedEvents
    case result of
      Left err -> return $ Left err
      Right wr -> do
        let decodeTagged (TaggedEvent meta enc) =
              case codec.decode enc of
                Just e -> (meta, e)
                Nothing -> throw $ DecodeError "publishingGlobalTaggedCodecEventStoreWriter" "Failed to decode tagged event payload"
        publish (toGlobalStreamEvents uuid wr decodeTagged taggedEvents)
        return $ Right wr
