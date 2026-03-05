module Eventium.ProjectionCache.Postgresql
  ( postgresqlProjectionCache,
    postgresqlGlobalProjectionCache,
    migrateProjectionSnapshot,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlPersistT)
import Eventium.ProjectionCache.Sql (migrateProjectionSnapshot, sqlGlobalProjectionCache, sqlProjectionCache)
import Eventium.ProjectionCache.Types (ProjectionCache, ProjectionName)
import Eventium.Store.Class (EventVersion, SequenceNumber)
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.UUID (UUID)

-- | PostgreSQL-backed 'ProjectionCache' for per-entity snapshots.
-- Alias for 'sqlProjectionCache'.
postgresqlProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
postgresqlProjectionCache = sqlProjectionCache

-- | PostgreSQL-backed 'ProjectionCache' for global blob snapshots.
-- Alias for 'sqlGlobalProjectionCache'.
postgresqlGlobalProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache () SequenceNumber JSONString (SqlPersistT m)
postgresqlGlobalProjectionCache = sqlGlobalProjectionCache
