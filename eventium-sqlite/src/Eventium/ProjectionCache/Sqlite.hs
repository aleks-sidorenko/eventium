module Eventium.ProjectionCache.Sqlite
  ( sqliteProjectionCache,
    sqliteGlobalProjectionCache,
    initializeSqliteProjectionCache,
    migrateProjectionSnapshot,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigrationSilent, runSqlPool)
import Eventium.ProjectionCache.Sql (migrateProjectionSnapshot, sqlGlobalProjectionCache, sqlProjectionCache)
import Eventium.ProjectionCache.Types (ProjectionCache, ProjectionName)
import Eventium.Store.Class (EventVersion, SequenceNumber)
import Eventium.Store.Sql.JSONString (JSONString)
import Eventium.UUID (UUID)

-- | SQLite-backed 'ProjectionCache' for per-entity snapshots.
-- Alias for 'sqlProjectionCache'.
sqliteProjectionCache ::
  (MonadIO m) =>
  ProjectionName ->
  ProjectionCache UUID EventVersion JSONString (SqlPersistT m)
sqliteProjectionCache = sqlProjectionCache

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
