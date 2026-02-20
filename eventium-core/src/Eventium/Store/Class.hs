{-# LANGUAGE RankNTypes #-}

module Eventium.Store.Class
  ( -- * EventStore
    EventStoreReader (..),
    EventStoreWriter (..),
    VersionedEventStoreReader,
    GlobalEventStoreReader,
    VersionedEventStoreWriter,
    runEventStoreReaderUsing,
    runEventStoreWriterUsing,
    module Eventium.Store.Queries,
    module Eventium.Store.Types,

    -- * Codec
    codecEventStoreReader,
    lenientCodecEventStoreReader,
    codecVersionedEventStoreReader,
    codecGlobalEventStoreReader,
    codecEventStoreWriter,
    metadataEnrichingEventStoreWriter,
    tagEvents,

    -- * Type embedding
    embeddedEventStoreWriter,

    -- * Utility functions
    transactionalExpectedWriteHelper,
  )
where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Functor.Contravariant
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, typeOf)
import Eventium.Codec
import Eventium.Store.Queries
import Eventium.Store.Types
import Eventium.TypeEmbedding
import Eventium.UUID

-- | An 'EventStoreReader' is a function to query a stream from an event store.
-- It operates in some monad @m@ and returns events of type @event@ from a
-- stream at @key@ ordered by @position@.
newtype EventStoreReader key position m event = EventStoreReader {getEvents :: QueryRange key position -> m [event]}

instance (Functor m) => Functor (EventStoreReader key position m) where
  fmap f (EventStoreReader reader) = EventStoreReader $ fmap (fmap f) <$> reader

type VersionedEventStoreReader m event = EventStoreReader UUID EventVersion m (VersionedStreamEvent event)

type GlobalEventStoreReader m event = EventStoreReader () SequenceNumber m (GlobalStreamEvent event)

-- | An 'EventStoreWriter' is a function to write some events of type @event@
-- to an event store in some monad @m@.
newtype EventStoreWriter key position m event
  = EventStoreWriter {storeEvents :: key -> ExpectedPosition position -> [event] -> m (Either (EventWriteError position) EventVersion)}

instance Contravariant (EventStoreWriter key position m) where
  contramap f (EventStoreWriter writer) = EventStoreWriter $ \key expectedPos -> writer key expectedPos . fmap f

type VersionedEventStoreWriter = EventStoreWriter UUID EventVersion

-- | Helper to create 'storeEventsRaw' given a function to get the latest
-- stream version and a function to write to the event store. **NOTE**: This
-- only works if the monad @m@ is transactional.
transactionalExpectedWriteHelper ::
  (Monad m, Ord position, Num position) =>
  (key -> m position) ->
  (key -> [event] -> m EventVersion) ->
  key ->
  ExpectedPosition position ->
  [event] ->
  m (Either (EventWriteError position) EventVersion)
transactionalExpectedWriteHelper getLatestVersion' storeEvents' key expected =
  go expected getLatestVersion' storeEvents' key
  where
    go AnyPosition = transactionalExpectedWriteHelper' Nothing
    go NoStream = transactionalExpectedWriteHelper' (Just $ (==) (-1))
    go StreamExists = transactionalExpectedWriteHelper' (Just (> (-1)))
    go (ExactPosition pos) = transactionalExpectedWriteHelper' (Just $ (==) pos)

transactionalExpectedWriteHelper' ::
  (Monad m) =>
  Maybe (position -> Bool) ->
  (key -> m position) ->
  (key -> [event] -> m EventVersion) ->
  key ->
  [event] ->
  m (Either (EventWriteError position) EventVersion)
transactionalExpectedWriteHelper' Nothing _ storeEvents' uuid events =
  storeEvents' uuid events <&> Right
transactionalExpectedWriteHelper' (Just f) getLatestVersion' storeEvents' uuid events = do
  latestVersion <- getLatestVersion' uuid
  if f latestVersion
    then storeEvents' uuid events <&> Right
    else return $ Left $ EventStreamNotAtExpectedVersion latestVersion

-- | Changes the monad an 'EventStoreReader' runs in. This is useful to run
-- event stores in another 'Monad' while forgetting the original 'Monad'.
runEventStoreReaderUsing ::
  (Monad m, Monad mstore) =>
  (forall a. mstore a -> m a) ->
  EventStoreReader key position mstore event ->
  EventStoreReader key position m event
runEventStoreReaderUsing runStore (EventStoreReader f) = EventStoreReader (runStore . f)

-- | Analog of 'runEventStoreReaderUsing' for a 'EventStoreWriter'.
runEventStoreWriterUsing ::
  (Monad m, Monad mstore) =>
  (forall a. mstore a -> m a) ->
  EventStoreWriter key position mstore event ->
  EventStoreWriter key position m event
runEventStoreWriterUsing runStore (EventStoreWriter f) =
  EventStoreWriter $ \key expectedPos events -> runStore $ f key expectedPos events

-- | Wraps an 'EventStoreReader' and transparently decodes events.
-- Throws 'DecodeError' if any event fails to decode.
-- Use 'lenientCodecEventStoreReader' to silently skip failures.
codecEventStoreReader ::
  (Monad m) =>
  Codec event encoded ->
  EventStoreReader key position m encoded ->
  EventStoreReader key position m event
codecEventStoreReader codec (EventStoreReader reader) =
  EventStoreReader $ fmap (map strictDecode) . reader
  where
    strictDecode s = case decode codec s of
      Just a -> a
      Nothing -> throw $ DecodeError "codecEventStoreReader" "Failed to decode event"

-- | Like 'codecEventStoreReader' but silently drops events that fail
-- to decode.
--
-- Recommended for production use when using sum-type codecs, as it
-- allows readers to gracefully handle events they don't recognize.
lenientCodecEventStoreReader ::
  (Monad m) =>
  Codec event encoded ->
  EventStoreReader key position m encoded ->
  EventStoreReader key position m event
lenientCodecEventStoreReader codec (EventStoreReader reader) =
  EventStoreReader $ fmap (mapMaybe (decode codec)) . reader

-- | Convenience wrapper around 'codecEventStoreReader' for
-- 'VersionedEventStoreReader'.
codecVersionedEventStoreReader ::
  (Monad m) =>
  Codec event encoded ->
  VersionedEventStoreReader m encoded ->
  VersionedEventStoreReader m event
codecVersionedEventStoreReader codec = codecEventStoreReader (traverseCodec codec)

-- | Convenience wrapper around 'codecEventStoreReader' for
-- 'GlobalEventStoreReader'.
codecGlobalEventStoreReader ::
  (Monad m) =>
  Codec event encoded ->
  GlobalEventStoreReader m encoded ->
  GlobalEventStoreReader m event
codecGlobalEventStoreReader codec = codecEventStoreReader (traverseCodec (traverseCodec codec))

-- | Like 'codecEventStoreReader' but for an 'EventStoreWriter'. Note that
-- 'EventStoreWriter' is an instance of 'Contravariant', so you can just use
-- @contramap (encode codec)@ instead of this function.
codecEventStoreWriter ::
  Codec event encoded ->
  EventStoreWriter key position m encoded ->
  EventStoreWriter key position m event
codecEventStoreWriter codec = contramap (encode codec)

-- | Wraps an 'EventStoreWriter' that accepts 'TaggedEvent's, producing a
-- writer that accepts domain events. Each event is encoded and tagged
-- with metadata (event type name derived from 'Typeable', current
-- UTC timestamp).
--
-- Use this instead of 'codecEventStoreWriter' when you want metadata
-- to be populated. The underlying writer must accept 'TaggedEvent's.
--
-- @
-- writer = metadataEnrichingEventStoreWriter myCodec taggedStore
-- @
metadataEnrichingEventStoreWriter ::
  (MonadIO m, Typeable event) =>
  Codec event encoded ->
  EventStoreWriter key position m (TaggedEvent encoded) ->
  EventStoreWriter key position m event
metadataEnrichingEventStoreWriter codec (EventStoreWriter write) =
  EventStoreWriter $ \key pos events -> do
    now <- liftIO getCurrentTime
    let tagged =
          map
            ( \e ->
                TaggedEvent
                  (EventMetadata (T.pack . show $ typeOf e) Nothing Nothing (Just now))
                  (encode codec e)
            )
            events
    write key pos tagged

-- | Tag events with metadata in a pure context. The caller supplies
-- the current time. Useful when 'MonadIO' is not available.
tagEvents ::
  (Typeable event) =>
  Codec event encoded ->
  UTCTime ->
  [event] ->
  [TaggedEvent encoded]
tagEvents codec now =
  map $ \e ->
    TaggedEvent
      (EventMetadata (T.pack . show $ typeOf e) Nothing Nothing (Just now))
      (encode codec e)

-- | Like 'codecEventStoreWriter' but uses a 'TypeEmbedding' instead of
-- a 'Codec'. Intended for embedding aggregate-specific event types into
-- an application-wide event sum type before writing to the store.
embeddedEventStoreWriter ::
  TypeEmbedding event adapted ->
  EventStoreWriter key position m adapted ->
  EventStoreWriter key position m event
embeddedEventStoreWriter emb = contramap (embed emb)
