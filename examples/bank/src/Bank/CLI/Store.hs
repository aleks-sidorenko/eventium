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
import Data.Text (Text, pack)
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
    dispatcher =
      commandHandlerDispatcher
        cliEventStoreWriter
        cliEventStoreReader
        [mkAggregateHandler accountBankCommandHandler formatAccountError]
    eventHandler =
      EventHandler (\event -> liftIO $ printJSONPretty (streamEventKey event, streamEventEvent event))
        <> processManagerEventHandler transferProcessManager cliGlobalEventStoreReader dispatcher

formatAccountError :: AccountCommandError -> Text
formatAccountError AccountAlreadyOpen = pack "Account already open"
formatAccountError InvalidInitialDeposit = pack "Invalid initial deposit"
formatAccountError (InsufficientFunds balance) = pack $ "Insufficient funds (balance: " ++ show balance ++ ")"
formatAccountError AccountNotOpen = pack "Account not open"

cliGlobalEventStoreReader :: (MonadIO m) => GlobalEventStoreReader (SqlPersistT m) BankEvent
cliGlobalEventStoreReader =
  codecGlobalEventStoreReader jsonStringCodec (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig {confIndent = Spaces 2})
