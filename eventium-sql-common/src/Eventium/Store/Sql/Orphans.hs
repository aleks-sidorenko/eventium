{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eventium.Store.Sql.Orphans
  (
  )
where

import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID
import Database.Persist
import Database.Persist.Sql
import Eventium.Store.Class
import Eventium.UUID

instance PersistField UUID where
  toPersistValue = PersistText . uuidToText
  fromPersistValue (PersistText t) =
    case uuidFromText t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue (PersistByteString bs) =
    maybe (Left "Invalid UUID") Right (uuidFromText (TE.decodeUtf8 bs))
  fromPersistValue (PersistLiteral_ _ bs) =
    maybe (Left "Invalid UUID") Right (uuidFromText (TE.decodeUtf8 bs))
  fromPersistValue v = Left $ "Expected UUID-compatible PersistValue, got: " <> T.pack (show v)

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PersistField EventVersion where
  toPersistValue (EventVersion n) = toPersistValue n
  fromPersistValue = fmap EventVersion . fromPersistValue

instance PersistFieldSql EventVersion where
  sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField SequenceNumber where
  toPersistValue (SequenceNumber n) = toPersistValue n
  fromPersistValue = fmap SequenceNumber . fromPersistValue

instance PersistFieldSql SequenceNumber where
  sqlType _ = sqlType (Proxy :: Proxy Int)
