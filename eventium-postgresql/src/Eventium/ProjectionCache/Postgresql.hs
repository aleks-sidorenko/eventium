module Eventium.ProjectionCache.Postgresql
  ( postgresqlVersionedProjectionCache,
    postgresqlGlobalProjectionCache,
    migrateProjectionSnapshot,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist.Sql (SqlPersistT)
import Eventium.ProjectionCache.Sql (ProjectionName, migrateProjectionSnapshot, sqlGlobalProjectionCache, sqlVersionedProjectionCache)
import Eventium.ProjectionCache.Types (ProjectionCache)
import Eventium.Store.Class (EventVersion, SequenceNumber)
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.UUID (UUID)

-- | PostgreSQL-backed 'ProjectionCache' for per-entity snapshots.
-- Alias for 'sqlVersionedProjectionCache'.
postgresqlVersionedProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
postgresqlVersionedProjectionCache = sqlVersionedProjectionCache

-- | PostgreSQL-backed 'ProjectionCache' for global blob snapshots.
-- Alias for 'sqlGlobalProjectionCache'.
postgresqlGlobalProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache () SequenceNumber JSONString (SqlPersistT m)
postgresqlGlobalProjectionCache = sqlGlobalProjectionCache
