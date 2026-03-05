module Eventium.ProjectionCache.Sqlite
  ( sqliteVersionedProjectionCache,
    sqliteGlobalProjectionCache,
    initializeSqliteProjectionCache,
    migrateProjectionSnapshot,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigrationSilent, runSqlPool)
import Eventium.ProjectionCache.Sql (ProjectionName, migrateProjectionSnapshot, sqlGlobalProjectionCache, sqlVersionedProjectionCache)
import Eventium.ProjectionCache.Types (ProjectionCache)
import Eventium.Store.Class (EventVersion, SequenceNumber)
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.UUID (UUID)

-- | SQLite-backed 'ProjectionCache' for per-entity snapshots.
-- Alias for 'sqlVersionedProjectionCache'.
sqliteVersionedProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
sqliteVersionedProjectionCache = sqlVersionedProjectionCache

-- | SQLite-backed 'ProjectionCache' for global blob snapshots.
-- Alias for 'sqlGlobalProjectionCache'.
sqliteGlobalProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache () SequenceNumber JSONString (SqlPersistT m)
sqliteGlobalProjectionCache = sqlGlobalProjectionCache

-- | Run migrations to create the projection_snapshots table in SQLite.
-- Mirrors 'initializeSqliteEventStore' from "Eventium.Store.Sqlite".
initializeSqliteProjectionCache ::
  (MonadIO m) =>
  ConnectionPool ->
  m ()
initializeSqliteProjectionCache pool =
  liftIO $ void $ runSqlPool (runMigrationSilent migrateProjectionSnapshot) pool
