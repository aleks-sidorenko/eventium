-- | Defines a universal event handler abstraction.
--
-- An 'EventHandler' is the simplest building block for consuming events.
-- It can be composed via 'Semigroup' (fan-out to multiple handlers),
-- adapted via 'Contravariant' (change the event type), and filtered via
-- 'eventHandlerMapMaybe'.
--
-- This abstraction is used by 'Eventium.EventPublisher.EventPublisher' for
-- synchronous dispatch and by 'Eventium.EventSubscription.EventSubscription'
-- for push-based delivery.
module Eventium.EventHandler
  ( EventHandler (..),
    handleEvents,
    eventHandlerMapMaybe,
    codecEventHandler,
    lenientCodecEventHandler,
    embeddedEventHandler,
  )
where

import Control.Exception (throw)
import Data.Foldable
import Data.Functor.Contravariant
import Eventium.Codec
import Eventium.TypeEmbedding

-- | An 'EventHandler' consumes events of type @event@ in some monad @m@.
--
-- Instances:
--
-- * 'Contravariant' — adapt the event type via @contramap@
-- * 'Semigroup' — fan-out: @h1 <> h2@ runs both handlers for each event
-- * 'Monoid' — @mempty@ is a no-op handler
newtype EventHandler m event = EventHandler {handleEvent :: event -> m ()}

instance Contravariant (EventHandler m) where
  contramap f (EventHandler h) = EventHandler (h . f)

instance (Applicative m) => Semigroup (EventHandler m event) where
  EventHandler h1 <> EventHandler h2 = EventHandler $ \e -> h1 e *> h2 e

instance (Applicative m) => Monoid (EventHandler m event) where
  mempty = EventHandler $ \_ -> pure ()

-- | Apply an 'EventHandler' to a list of events in order.
handleEvents :: (Monad m) => EventHandler m event -> [event] -> m ()
handleEvents (EventHandler h) = mapM_ h

-- | Filter events before they reach the handler. Events for which the
-- function returns 'Nothing' are silently dropped.
eventHandlerMapMaybe ::
  (Applicative m) =>
  (eventB -> Maybe eventA) ->
  EventHandler m eventA ->
  EventHandler m eventB
eventHandlerMapMaybe f (EventHandler h) = EventHandler $ \e -> for_ (f e) h

-- | Wrap an 'EventHandler' with a 'Codec' so it can consume events
-- of the encoded type. Throws 'DecodeError' if decoding fails.
-- Use 'lenientCodecEventHandler' to silently skip failures.
codecEventHandler ::
  (Applicative m) =>
  Codec event encoded ->
  EventHandler m event ->
  EventHandler m encoded
codecEventHandler codec (EventHandler h) = EventHandler $ \e ->
  case decode codec e of
    Just a -> h a
    Nothing -> throw $ DecodeError "codecEventHandler" "Failed to decode event"

-- | Like 'codecEventHandler' but silently drops events that fail to
-- decode.
--
-- Recommended for production use when using sum-type codecs, as it
-- allows event handlers to gracefully handle events they don't recognize.
lenientCodecEventHandler ::
  (Applicative m) =>
  Codec event encoded ->
  EventHandler m event ->
  EventHandler m encoded
lenientCodecEventHandler codec = eventHandlerMapMaybe (decode codec)

-- | Adapt an 'EventHandler' using a 'TypeEmbedding'. Events that do not
-- belong to the embedded subset are silently dropped.
embeddedEventHandler ::
  (Applicative m) =>
  TypeEmbedding event adapted ->
  EventHandler m event ->
  EventHandler m adapted
embeddedEventHandler emb = eventHandlerMapMaybe (extract emb)
