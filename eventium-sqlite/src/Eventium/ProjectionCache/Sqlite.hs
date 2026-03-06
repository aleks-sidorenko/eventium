module Eventium.ProjectionCache.Sqlite
  ( sqliteVersionedProjectionCache,
    sqliteGlobalProjectionCache,
    sqliteCheckpointStore,
    CheckpointName (..),
    initializeSqliteProjectionCache,
    migrateProjectionSnapshot,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigrationSilent, runSqlPool)
import Eventium.EventSubscription (CheckpointStore)
import Eventium.ProjectionCache.Sql (CheckpointName (..), ProjectionName, migrateProjectionSnapshot, sqlCheckpointStore, sqlGlobalProjectionCache, sqlVersionedProjectionCache)
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

-- | SQLite-backed 'CheckpointStore' for tracking subscription position.
-- Alias for 'sqlCheckpointStore'.
sqliteCheckpointStore ::
  (MonadIO m) =>
  CheckpointName ->
  CheckpointStore (SqlPersistT m) SequenceNumber
sqliteCheckpointStore = sqlCheckpointStore

-- | Run migrations to create the projection_snapshots table in SQLite.
-- Mirrors 'initializeSqliteEventStore' from "Eventium.Store.Sqlite".
initializeSqliteProjectionCache ::
  (MonadIO m) =>
  ConnectionPool ->
  m ()
initializeSqliteProjectionCache pool =
  liftIO $ void $ runSqlPool (runMigrationSilent migrateProjectionSnapshot) pool
