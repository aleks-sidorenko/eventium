{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Eventium.ProjectionCache.Sql
  ( ProjectionName (..),
    ProjectionSnapshotEntity (..),
    migrateProjectionSnapshot,
    sqlVersionedProjectionCache,
    sqlGlobalProjectionCache,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Eventium.ProjectionCache.Types
import Eventium.Store.Class (EventVersion (..), SequenceNumber (..))
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.Store.Sql.Orphans ()
import Eventium.UUID (UUID, nil)

-- | A name identifying a projection in the projection cache.
-- Used as a discriminator so multiple projections can share one storage table.
newtype ProjectionName = ProjectionName Text
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql)

share
  [mkPersist sqlSettings {mpsFieldLabelModifier = const id}, mkMigrate "migrateProjectionSnapshot"]
  [persistLowerCase|
ProjectionSnapshotEntity sql=projection_snapshots
    projectionName ProjectionName
    entityId UUID sql=key
    position Int
    state JSONString
    updatedAt UTCTime
    Primary projectionName entityId
    deriving Show
|]

-- | A SQL-backed 'ProjectionCache' for per-entity snapshots.
--
-- Stores one row per entity. The @key@ is a UUID (aggregate/entity ID)
-- and @position@ is an EventVersion.
--
-- Composes with 'codecProjectionCache' for state encoding.
-- Works with both PostgreSQL and SQLite via Persistent.
sqlVersionedProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
sqlVersionedProjectionCache name =
  ProjectionCache
    { storeSnapshot = \uuid (EventVersion ver) state -> do
        now <- liftIO getCurrentTime
        repsert (ProjectionSnapshotEntityKey name uuid) $
          ProjectionSnapshotEntity
            { projectionName = name,
              entityId = uuid,
              position = ver,
              state = state,
              updatedAt = now
            },
      loadSnapshot = \uuid -> do
        mEntity <- get (ProjectionSnapshotEntityKey name uuid)
        return $ fmap (\e -> (EventVersion e.position, e.state)) mEntity
    }

-- | A SQL-backed 'ProjectionCache' for global blob snapshots.
--
-- Stores one row per read model using 'nil' UUID as the key.
-- The @position@ is a SequenceNumber.
--
-- Composes with 'codecProjectionCache' for state encoding.
-- Works with both PostgreSQL and SQLite via Persistent.
sqlGlobalProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache () SequenceNumber JSONString (SqlPersistT m)
sqlGlobalProjectionCache name =
  ProjectionCache
    { storeSnapshot = \() (SequenceNumber sn) state -> do
        now <- liftIO getCurrentTime
        repsert (ProjectionSnapshotEntityKey name nil) $
          ProjectionSnapshotEntity
            { projectionName = name,
              entityId = nil,
              position = sn,
              state = state,
              updatedAt = now
            },
      loadSnapshot = \() -> do
        mEntity <- get (ProjectionSnapshotEntityKey name nil)
        return $ fmap (\e -> (SequenceNumber e.position, e.state)) mEntity
    }
