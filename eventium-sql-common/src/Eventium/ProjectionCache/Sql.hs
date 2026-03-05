{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Eventium.ProjectionCache.Sql
  ( ProjectionSnapshotEntity (..),
    migrateProjectionSnapshot,
    sqlProjectionCache,
    sqlGlobalProjectionCache,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Eventium.ProjectionCache.Types
import Eventium.Store.Class (EventVersion (..), SequenceNumber (..))
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.Store.Sql.Orphans ()
import Eventium.UUID (UUID, nil)

share
  [mkPersist sqlSettings, mkMigrate "migrateProjectionSnapshot"]
  [persistLowerCase|
ProjectionSnapshotEntity sql=projection_snapshots
    projectionName ProjectionName
    entityId UUID sql=key
    position Int
    state JSONString
    updatedAt UTCTime default=now()
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
sqlProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
sqlProjectionCache name =
  ProjectionCache
    { storeProjectionSnapshot = \uuid (EventVersion ver) state -> do
        now <- liftIO getCurrentTime
        repsert (ProjectionSnapshotEntityKey name uuid) $
          ProjectionSnapshotEntity
            { projectionSnapshotEntityProjectionName = name,
              projectionSnapshotEntityEntityId = uuid,
              projectionSnapshotEntityPosition = ver,
              projectionSnapshotEntityState = state,
              projectionSnapshotEntityUpdatedAt = now
            },
      loadProjectionSnapshot = \uuid -> do
        mEntity <- get (ProjectionSnapshotEntityKey name uuid)
        return $ fmap (\(ProjectionSnapshotEntity _ _ pos st _) -> (EventVersion pos, st)) mEntity
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
    { storeProjectionSnapshot = \() (SequenceNumber sn) state -> do
        now <- liftIO getCurrentTime
        repsert (ProjectionSnapshotEntityKey name nil) $
          ProjectionSnapshotEntity
            { projectionSnapshotEntityProjectionName = name,
              projectionSnapshotEntityEntityId = nil,
              projectionSnapshotEntityPosition = sn,
              projectionSnapshotEntityState = state,
              projectionSnapshotEntityUpdatedAt = now
            },
      loadProjectionSnapshot = \() -> do
        mEntity <- get (ProjectionSnapshotEntityKey name nil)
        return $ fmap (\(ProjectionSnapshotEntity _ _ pos st _) -> (SequenceNumber pos, st)) mEntity
    }
