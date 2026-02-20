{-# LANGUAGE RecordWildCards #-}

module Eventium.Projection
  ( Projection (..),
    latestProjection,
    allProjections,
    StreamProjection (..),
    VersionedStreamProjection,
    GlobalStreamProjection,
    streamProjection,
    versionedStreamProjection,
    globalStreamProjection,
    streamProjectionEventHandler,
    getLatestStreamProjection,
    codecProjection,
    lenientCodecProjection,
    embeddedProjection,
    projectionMapMaybe,
  )
where

import Control.Exception (throw)
import Data.Foldable (foldl')
import Data.Functor.Contravariant
import Data.List (scanl')
import Eventium.Codec
import Eventium.Store.Class
import Eventium.TypeEmbedding
import Eventium.UUID

-- | A 'Projection' is a piece of @state@ that is constructed only from
-- @event@s. A Projection is how you reconstruct event sourced state from the
-- ordered stream of events that constitute that state. The "seed" of a
-- Projection is the initial state before any events are applied. The event
-- handler for a projection is the function that actually modifies state based
-- on the given event.
data Projection state event
  = Projection
  { -- | Initial state of a projection
    projectionSeed :: state,
    -- | The function that applies and event to the current state, producing a
    -- new state.
    projectionEventHandler :: state -> event -> state
  }

instance Contravariant (Projection state) where
  contramap f (Projection seed handler) = Projection seed handler'
    where
      handler' state event = handler state (f event)

-- | Computes the latest state of a 'Projection' from some events.
latestProjection :: (Foldable t) => Projection state event -> t event -> state
latestProjection (Projection seed handler) = foldl' handler seed

-- | Given a list of events, produce all the Projections that were ever
-- produced. Just a 'scanl' using 'projectionEventHandler'. This function is
-- useful for testing 'Projection's; you can easily assert that all the states
-- of a Projection are valid given a list of events.
allProjections :: Projection state event -> [event] -> [state]
allProjections (Projection seed handler) = scanl' handler seed

-- | A 'StreamProjection' is a 'Projection' that has been constructed from
-- events from a particular event stream. This is useful when we want to cache
-- the resulting state and also keep track of what part of the stream the state
-- is caught up to.
data StreamProjection key position state event
  = StreamProjection
  { streamProjectionKey :: !key,
    streamProjectionPosition :: !position,
    streamProjectionProjection :: !(Projection state event),
    streamProjectionState :: !state
  }

type VersionedStreamProjection = StreamProjection UUID EventVersion

type GlobalStreamProjection state event = StreamProjection () SequenceNumber state (VersionedStreamEvent event)

-- | Initialize a 'StreamProjection' with a 'Projection', key, and order key.
streamProjection ::
  key ->
  position ->
  Projection state event ->
  StreamProjection key position state event
streamProjection key position projection@Projection {..} =
  StreamProjection key position projection projectionSeed

-- | Initialize a 'VersionedStreamProjection'.
versionedStreamProjection ::
  UUID ->
  Projection state event ->
  VersionedStreamProjection state event
versionedStreamProjection uuid = streamProjection uuid (-1)

-- | Initialize a 'GlobalStreamProjection'.
globalStreamProjection ::
  Projection state (VersionedStreamEvent event) ->
  GlobalStreamProjection state event
globalStreamProjection = streamProjection () 0

-- | Apply an event to the 'StreamProjection'. NOTE: There is no guarantee that
-- the order key for the event is greater than the current order key in the
-- 'StreamProjection'. This function will simply update the 'StreamProjection'
-- to use the order key of the event.
streamProjectionEventHandler ::
  StreamProjection key position state event ->
  StreamEvent eventKey position event ->
  StreamProjection key position state event
streamProjectionEventHandler StreamProjection {..} streamEvent =
  let Projection {..} = streamProjectionProjection
      position' = streamEventPosition streamEvent
      state' = projectionEventHandler streamProjectionState (streamEventEvent streamEvent)
   in StreamProjection streamProjectionKey position' streamProjectionProjection state'

-- | Gets the latest projection from a store by querying events from the latest
-- order key and then applying the events using the Projection's event handler.
getLatestStreamProjection ::
  (Monad m, Num position) =>
  EventStoreReader key position m (StreamEvent key position event) ->
  StreamProjection key position state event ->
  m (StreamProjection key position state event)
getLatestStreamProjection (EventStoreReader getEvents') projection@StreamProjection {..} = do
  events <- getEvents' (eventsStartingAt streamProjectionKey $ streamProjectionPosition + 1)
  return $ foldl' streamProjectionEventHandler projection events

-- | Use a 'Codec' to wrap a 'Projection' with event type @event@ so it
-- uses the @encoded@ type. Throws 'DecodeError' if decoding
-- fails. Use 'lenientCodecProjection' to silently skip unrecognized events.
codecProjection ::
  Codec event encoded ->
  Projection state event ->
  Projection state encoded
codecProjection codec (Projection seed handler) =
  Projection seed handler'
  where
    handler' state encoded =
      case decode codec encoded of
        Just event -> handler state event
        Nothing -> throw $ DecodeError "codecProjection" "Failed to decode event"

-- | Like 'codecProjection' but silently drops events that fail to
-- decode. Useful for projections that only care about a subset of event
-- types (e.g. when using a sum-type codec).
--
-- Recommended for production use when using sum-type codecs, as it
-- allows projections to gracefully handle events they don't recognize.
lenientCodecProjection ::
  Codec event encoded ->
  Projection state event ->
  Projection state encoded
lenientCodecProjection codec = projectionMapMaybe (decode codec)

-- | Adapt a 'Projection' using a 'TypeEmbedding'. Events that do not belong
-- to the embedded subset are silently skipped. This is the correct semantics
-- for type embeddings, where it is expected that many events in the superset
-- will not match the subset.
embeddedProjection ::
  TypeEmbedding event adapted ->
  Projection state event ->
  Projection state adapted
embeddedProjection emb = projectionMapMaybe (extract emb)

-- | Transform a 'Projection' when you only have a partial relationship between
-- the source event type and the target event type.
projectionMapMaybe ::
  (eventB -> Maybe eventA) ->
  Projection state eventA ->
  Projection state eventB
projectionMapMaybe f (Projection seed handler) = Projection seed handler'
  where
    handler' state = maybe state (handler state) . f
