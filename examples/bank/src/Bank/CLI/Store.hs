{-# LANGUAGE RecordWildCards #-}

module Bank.CLI.Store
  ( runDB,
    cliEventStoreReader,
    cliEventStoreWriter,
    cliGlobalEventStoreReader,
    printJSONPretty,
  )
where

import Bank.Models
import Bank.ProcessManagers.TransferManager
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Database.Persist.Sqlite
import Eventium
import Eventium.Store.Sqlite

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

cliEventStoreReader :: (MonadIO m) => VersionedEventStoreReader (SqlPersistT m) BankEvent
cliEventStoreReader = codecVersionedEventStoreReader jsonStringCodec $ sqlEventStoreReader defaultSqlEventStoreConfig

cliEventStoreWriter :: (MonadIO m) => VersionedEventStoreWriter (SqlPersistT m) BankEvent
cliEventStoreWriter =
  publishingEventStoreWriter writer (synchronousPublisher eventHandler)
  where
    sqlStore = sqliteEventStoreWriter defaultSqlEventStoreConfig
    writer = codecEventStoreWriter jsonStringCodec sqlStore
    eventHandler = EventHandler $ \event -> do
      liftIO $ printJSONPretty (streamEventKey event, streamEventEvent event)
      runTransferManager event

runTransferManager :: (MonadIO m) => VersionedStreamEvent BankEvent -> SqlPersistT m ()
runTransferManager newEvent = do
  let projection = processManagerProjection transferProcessManager
      globalProjection = globalStreamProjection projection
  StreamProjection {..} <- getLatestStreamProjection cliGlobalEventStoreReader globalProjection
  let effects = processManagerReact transferProcessManager streamProjectionState newEvent
      dispatch uuid cmd = do
        _ <- applyCommandHandler cliEventStoreWriter cliEventStoreReader accountBankCommandHandler uuid cmd
        return ()
  runProcessManagerEffects dispatch effects

cliGlobalEventStoreReader :: (MonadIO m) => GlobalEventStoreReader (SqlPersistT m) BankEvent
cliGlobalEventStoreReader =
  codecGlobalEventStoreReader jsonStringCodec (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig {confIndent = Spaces 2})
