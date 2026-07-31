{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.Sql.JSONString
  ( JSONString,
    jsonStringCodec,
    upcastingJsonStringCodec,
    encodeJSON,
    decodeJSON,
  )
where

import qualified Data.Aeson as Aeson
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import Database.Persist
import Database.Persist.Sql
import Eventium.Codec (Codec (..), composeCodecs)
import Eventium.SchemaEvolution.Json (upcastingValueCodec)
import Eventium.SchemaEvolution.Types (EventTypeName, SchemaRegistry)

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString Text
  deriving (Eq, PersistField)

instance PersistFieldSql JSONString where
  sqlType _ = SqlOther "jsonb"

instance Show JSONString where
  show (JSONString t) = show t

jsonStringCodec :: (Aeson.ToJSON a, Aeson.FromJSON a) => Codec a JSONString
jsonStringCodec =
  Codec
    encodeJSON
    decodeJSON

-- | Like 'jsonStringCodec', but applies event schema evolution
-- (upcast-on-read) between the domain type and the stored JSON. Drop-in
-- replacement for 'jsonStringCodec' at reader/writer call sites: writes are
-- wrapped in the current-version envelope, and reads normalize older stored
-- events to the current shape via the 'SchemaRegistry'. The @eventTypeOf@
-- function reads the event-type name from a payload (the app's tagging
-- convention).
--
-- 'jsonStringCodec' specialises to @Codec 'Aeson.Value' JSONString@ (a 'Value'
-- is trivially 'Aeson.ToJSON'\/'Aeson.FromJSON'), so this is just the
-- value-level upcasting codec composed with the text (de)serialisation.
upcastingJsonStringCodec ::
  (Aeson.ToJSON a, Aeson.FromJSON a) =>
  (Aeson.Value -> Maybe EventTypeName) ->
  SchemaRegistry Aeson.Value ->
  Codec a JSONString
upcastingJsonStringCodec eventTypeOf registry =
  composeCodecs (upcastingValueCodec eventTypeOf registry) jsonStringCodec

encodeJSON :: (Aeson.ToJSON a) => a -> JSONString
encodeJSON = JSONString . TLE.decodeUtf8 . Aeson.encode

decodeJSON :: (Aeson.FromJSON a) => JSONString -> Maybe a
decodeJSON (JSONString t) = Aeson.decode . TLE.encodeUtf8 $ t
