{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition for a default Entity to use with a SQL event store.
module Eventium.Store.Sql.DefaultEntity
  ( SqlEvent (..),
    SqlEventId,
    migrateSqlEvent,
    defaultSqlEventStoreConfig,
  )
where

import Database.Persist (Key)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.TH
import Eventium.Store.Class
import Eventium.Store.Sql.JSONString
import Eventium.Store.Sql.Operations
import Eventium.Store.Sql.Orphans ()
import Eventium.UUID

share
  [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"]
  [persistLowerCase|
SqlEvent sql=events
    uuid UUID
    version EventVersion
    payload JSONString
    metadata JSONString Maybe
    UniqueUuidVersion uuid version
    deriving Show
|]

sqlEventMakeKey :: SequenceNumber -> Key SqlEvent
sqlEventMakeKey (SequenceNumber n) =
  toSqlKey (fromIntegral n)

sqlEventUnKey :: Key SqlEvent -> SequenceNumber
sqlEventUnKey key =
  SequenceNumber (fromIntegral (fromSqlKey key))

defaultSqlEventStoreConfig :: SqlEventStoreConfig SqlEvent JSONString
defaultSqlEventStoreConfig =
  SqlEventStoreConfig
    { sequenceMakeEntity = SqlEvent,
      makeKey = sqlEventMakeKey,
      unKey = sqlEventUnKey,
      uuid = \(SqlEvent u _ _ _) -> u,
      version = \(SqlEvent _ v _ _) -> v,
      eventData = \(SqlEvent _ _ p _) -> p,
      eventMetadata = \(SqlEvent _ _ _ m) -> m,
      sequenceNumberField = SqlEventId,
      uuidField = SqlEventUuid,
      versionField = SqlEventVersion,
      dataField = SqlEventPayload
    }
