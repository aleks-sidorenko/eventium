{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Eventium.Codec
  ( -- * Class
    Codec (..),
    composeCodecs,

    -- * Common codecs
    idCodec,
    traverseCodec,
    jsonCodec,
    jsonTextCodec,
    dynamicCodec,

    -- * Sum types
    EventSumType (..),
    eventSumTypeCodec,

    -- * Exceptions
    DecodeError (..),
    EncodeError (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.Dynamic
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable (typeOf)
import GHC.Generics

-- | A 'Codec' describes the injective conversion between types @a@ and
-- @b@. In plain English, this means that you can go from @a@ to @b@, and you
-- can 'Maybe' go from @b@ back to @a@. This is often used to encode events
-- to an event store, and then decode them back.
data Codec a b = Codec
  { encode :: a -> b,
    decode :: b -> Maybe a
  }

-- | Exception thrown when a strict decode function encounters
-- data it cannot decode. Can be caught specifically using
-- @Control.Exception.catch@ or @Control.Exception.try@.
--
-- In production, prefer the lenient variants (e.g. 'lenientCodecProjection',
-- 'lenientCodecEventHandler', 'lenientCodecEventStoreReader') which
-- silently skip unrecognized events instead of throwing.
data DecodeError = DecodeError
  { decodeErrorContext :: !String,
    decodeErrorMessage :: !String
  }
  deriving (Show, Eq)

instance Exception DecodeError

-- | Exception thrown when 'eventSumTypeCodec' cannot encode a value.
-- This indicates a programming error: the source sum type contains a
-- constructor not present in the target sum type.
data EncodeError = EncodeError
  { encodeErrorContext :: !String,
    encodeErrorMessage :: !String
  }
  deriving (Show, Eq)

instance Exception EncodeError

-- | Apply an intermediate 'Codec' to a codec to go from type @a@ to
-- @c@ with @b@ in the middle. Note that with decoding, if the conversion
-- from @c@ to @b@ or from @b@ to @a@ fails, the whole decoding fails.
composeCodecs :: Codec a b -> Codec b c -> Codec a c
composeCodecs codec1 codec2 = Codec encode' decode'
  where
    encode' = encode codec2 . encode codec1
    decode' x = decode codec2 x >>= decode codec1

-- | Simple "codec" using 'id'. Useful for when an API requires a
-- codec but you don't need to actually change types.
idCodec :: Codec a a
idCodec = Codec id Just

-- | Uses 'Traversable' to wrap a 'Codec'.
traverseCodec ::
  (Traversable t) =>
  Codec a b ->
  Codec (t a) (t b)
traverseCodec Codec {..} =
  Codec encode' decode'
  where
    encode' = fmap encode
    decode' = traverse decode

-- | A 'Codec' for aeson 'Value's.
jsonCodec :: (Typeable a, ToJSON a, FromJSON a) => Codec a Value
jsonCodec =
  Codec
    { encode = toJSON,
      decode = \x ->
        case fromJSON x of
          Success a -> Just a
          Error _ -> Nothing
    }

-- | A 'Codec' to convert JSON to/from lazy text. Useful for Sql event
-- stores that store JSON values as text.
jsonTextCodec :: (Typeable a, ToJSON a, FromJSON a) => Codec a TL.Text
jsonTextCodec =
  Codec
    { encode = TLE.decodeUtf8 . Aeson.encode,
      decode = Aeson.decode . TLE.encodeUtf8
    }

-- | A 'Codec' for 'Dynamic' values using 'toDyn' and 'fromDynamic'.
dynamicCodec :: (Typeable a) => Codec a Dynamic
dynamicCodec = Codec toDyn fromDynamic

-- | A 'Codec' from one 'EventSumType' instance to another. WARNING: If
-- not all events in the source 'EventSumType' are in the @encoded@
-- 'EventSumType', then this function will be partial!
eventSumTypeCodec :: (Typeable a, EventSumType a, EventSumType b) => Codec a b
eventSumTypeCodec = Codec encode' decode'
  where
    encode' event =
      fromMaybe
        (throw $ EncodeError "eventSumTypeCodec" ("Can't encode event of type " ++ show (typeOf event)))
        (eventFromDyn $ eventToDyn event)
    decode' = eventFromDyn . eventToDyn

-- | This is a type class for encoding sum types of events to 'Dynamic'
-- without the associated constructor. This is useful when transforming between
-- two sum types of events. A common pattern is to put all the events in an
-- application in one big event sum type, and then have a smaller sum type for
-- each 'Projection'. Then, you can use 'eventSumTypeCodec' to transform
-- between the two.
--
-- It is meant to be derived with 'Generic'. For example:
--
-- @
--    data EventA = EventA deriving (Show)
--    data EventB = EventB deriving (Show)
--    data EventC = EventC deriving (Show)
--
--    data AllEvents
--      = AllEventsEventA EventA
--      | AllEventsEventB EventB
--      | AllEventsEventC EventC
--      deriving (Show, Generic)
--
--    instance EventSumType AllEvents
--
--    data MyEvents
--      = MyEventsEventA EventA
--      | MyEventsEventB EventB
--      deriving (Show, Generic)
--
--    instance EventSumType MyEvents
-- @
--
-- Now we can encode to 'Dynamic' without a constructor tag:
--
-- >>> eventToDyn (MyEventsEventA EventA)
-- <<EventA>>
--
-- We can also go from a 'MyEvents' value to an 'AllEvents' value:
--
-- >>> eventFromDyn (eventToDyn (MyEventsEventA EventA)) :: Maybe AllEvents
-- Just (AllEventsEventA EventA)
class EventSumType a where
  -- | Convert an event to a 'Dynamic' without the constructor tag
  eventToDyn :: a -> Dynamic

  -- | Go from a 'Dynamic' to an event with the constructor tag. Note, this
  -- function is @O(n)@ to the number of constructors.
  eventFromDyn :: Dynamic -> Maybe a

  default eventToDyn :: (Generic a, EventSumType' (Rep a)) => a -> Dynamic
  eventToDyn x = eventToDyn' (from x)

  default eventFromDyn :: (Generic a, EventSumType' (Rep a)) => Dynamic -> Maybe a
  eventFromDyn = fmap to . eventFromDyn'

-- Auxiliary type class for 'EventSumType' Generic fun
class EventSumType' f where
  eventToDyn' :: f p -> Dynamic
  eventFromDyn' :: Dynamic -> Maybe (f p)

-- M1 is the top-level metadata. We don't need the metadata so we just pass on
-- through.
instance (EventSumType' f) => EventSumType' (M1 i t f) where
  eventToDyn' (M1 x) = eventToDyn' x
  eventFromDyn' = fmap M1 . eventFromDyn'

-- The :+: operator is for when a type has multiple constructors. When
-- encoding, we just pass on through. When decoding, we try the first
-- constructor, and if that fails then the second.
instance (EventSumType' f, EventSumType' g) => EventSumType' (f :+: g) where
  eventToDyn' (L1 x) = eventToDyn' x
  eventToDyn' (R1 x) = eventToDyn' x
  eventFromDyn' dyn = (L1 <$> eventFromDyn' dyn) <|> (R1 <$> eventFromDyn' dyn)

-- K1 R represents an actual constructor. This is where we do the actual
-- conversion to/from 'Dynamic'.
instance (Typeable c) => EventSumType' (K1 R c) where
  eventToDyn' (K1 x) = toDyn x
  eventFromDyn' dyn = K1 <$> fromDynamic dyn
