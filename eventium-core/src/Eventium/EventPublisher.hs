{-# LANGUAGE OverloadedStrings #-}

-- | Defines an event publisher abstraction that decouples event notification
-- from event storage.
--
-- An 'EventPublisher' is responsible for distributing newly stored events to
-- interested consumers using breadth-first dispatch.
--
-- Use 'publishingEventStoreWriter' to wrap an 'EventStoreWriter' so that
-- events are automatically published after a successful write.
module Eventium.EventPublisher
  ( EventPublisher (..),
    synchronousPublisher,
    publishingEventStoreWriter,
    publishingEventStoreWriterTagged,
    publishingEventStoreWriterTaggedCodec,
  )
where

import Control.Exception (throw)
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
      Right endVersion -> do
        let startVersion = endVersion - fromIntegral (length events) + 1
            versionedEvents = zipWith (\v e -> StreamEvent uuid v (emptyMetadata "") e) [startVersion ..] events
        publish uuid versionedEvents
        return $ Right endVersion

-- | Like 'publishingEventStoreWriter' but for writers that accept
-- 'TaggedEvent's. The metadata from each tagged event is preserved
-- in the 'StreamEvent' wrappers passed to the publisher.
publishingEventStoreWriterTagged ::
  (Monad m) =>
  VersionedEventStoreWriter m (TaggedEvent event) ->
  EventPublisher m event ->
  VersionedEventStoreWriter m (TaggedEvent event)
publishingEventStoreWriterTagged (EventStoreWriter write) (EventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos taggedEvents -> do
    result <- write uuid expectedPos taggedEvents
    case result of
      Left err -> return $ Left err
      Right endVersion -> do
        let startVersion = endVersion - fromIntegral (length taggedEvents) + 1
            versionedEvents =
              zipWith
                (\v (TaggedEvent meta e) -> StreamEvent uuid v meta e)
                [startVersion ..]
                taggedEvents
        publish uuid versionedEvents
        return $ Right endVersion

-- | Like 'publishingEventStoreWriterTagged' but decodes each tagged event's
-- payload through a 'Codec' before publishing. This lets the writer accept
-- @TaggedEvent encoded@ (e.g. @TaggedEvent JSONString@) while handlers
-- receive decoded domain events.
--
-- Use this when the tagged writer stores serialized payloads but event
-- handlers (read models, process managers) need domain event types.
--
-- Throws 'DecodeError' if any event fails to decode.
publishingEventStoreWriterTaggedCodec ::
  (Monad m) =>
  Codec event encoded ->
  VersionedEventStoreWriter m (TaggedEvent encoded) ->
  EventPublisher m event ->
  VersionedEventStoreWriter m (TaggedEvent encoded)
publishingEventStoreWriterTaggedCodec codec (EventStoreWriter write) (EventPublisher publish) =
  EventStoreWriter $ \uuid expectedPos taggedEvents -> do
    result <- write uuid expectedPos taggedEvents
    case result of
      Left err -> return $ Left err
      Right endVersion -> do
        let startVersion = endVersion - fromIntegral (length taggedEvents) + 1
            versionedEvents =
              zipWith
                ( \v (TaggedEvent meta enc) ->
                    let event = case codec.decode enc of
                          Just e -> e
                          Nothing -> throw $ DecodeError "publishingEventStoreWriterTaggedCodec" "Failed to decode tagged event payload"
                     in StreamEvent uuid v meta event
                )
                [startVersion ..]
                taggedEvents
        publish uuid versionedEvents
        return $ Right endVersion
