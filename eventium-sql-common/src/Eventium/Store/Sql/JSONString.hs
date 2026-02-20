{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.Sql.JSONString
  ( JSONString,
    jsonStringCodec,
  )
where

import qualified Data.Aeson as Aeson
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import Database.Persist
import Database.Persist.Sql
import Eventium.Codec (Codec (..))

-- | A more specific type than just ByteString for JSON data.
newtype JSONString = JSONString {unJSONString :: Text}
  deriving (Eq, PersistField)

instance PersistFieldSql JSONString where
  sqlType _ = SqlOther "jsonb"

instance Show JSONString where
  show = show . unJSONString

jsonStringCodec :: (Aeson.ToJSON a, Aeson.FromJSON a) => Codec a JSONString
jsonStringCodec =
  Codec
    encodeJSON
    decodeJSON

encodeJSON :: (Aeson.ToJSON a) => a -> JSONString
encodeJSON = JSONString . TLE.decodeUtf8 . Aeson.encode

decodeJSON :: (Aeson.FromJSON a) => JSONString -> Maybe a
decodeJSON = Aeson.decode . TLE.encodeUtf8 . unJSONString
