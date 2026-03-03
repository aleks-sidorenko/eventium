module Cafe.CLI
  ( cliMain,
    printJSONPretty,
  )
where

import Cafe.CLI.Options
import Cafe.CLI.Transformer
import Cafe.DB
import Cafe.Models.Tab
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (pack)
import Database.Persist.Sqlite
import Eventium
import Eventium.Store.Sqlite
import Safe

cliMain :: IO ()
cliMain = do
  opts <- runOptionsParser

  -- Set up DB connection
  pool <- runNoLoggingT $ createSqlitePool (pack opts.databaseFile) 1
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  void $ liftIO $ runSqlPool (runMigrationSilent migrateTabEntity) pool

  runCLI pool (runCLICommand opts.command)

runCLICommand :: Command -> CLI ()
runCLICommand OpenTab = do
  (key, uuid) <- runDB openTab
  liftIO $ putStrLn $ "Opened tab. Id: " ++ show (fromSqlKey key) ++ ", UUID: " ++ show uuid
runCLICommand ListMenu = liftIO $ do
  putStrLn "Food:"
  let printPair (i, MenuItem desc price) = putStrLn $ show i ++ ": " ++ desc ++ " ($" ++ show price ++ ")"
  mapM_ printPair (zip [0 :: Int ..] $ map (\(Food mi) -> mi) allFood)
  putStrLn "Drinks:"
  mapM_ printPair (zip [0 :: Int ..] $ map (\(Drink mi) -> mi) allDrinks)
runCLICommand (ViewTab tabId) = do
  uuid <- fromJustNote "Could not find tab with given id" <$> runDB (getTabUuid tabId)
  sp <- runDB $ getLatestStreamProjection cliEventStoreReader (versionedStreamProjection uuid (codecProjection jsonStringCodec tabProjection))
  liftIO $ printJSONPretty sp.state
runCLICommand (TabCommand tabId command) = do
  uuid <- fromJustNote "Could not find tab with given id" <$> runDB (getTabUuid tabId)
  result <-
    runDB $
      applyCommandHandler
        cliEventStore
        cliEventStoreReader
        (codecCommandHandler jsonStringCodec idCodec tabCommandHandler)
        uuid
        command
  case result of
    Left err -> liftIO . putStrLn $ "Error: " ++ show err
    Right events -> do
      liftIO . putStrLn $ "Events: " ++ show events
      sp <- runDB $ getLatestStreamProjection cliEventStoreReader (versionedStreamProjection uuid (codecProjection jsonStringCodec tabProjection))
      liftIO . putStrLn $ "Latest state:"
      liftIO $ printJSONPretty sp.state

printJSONPretty :: (ToJSON a) => a -> IO ()
printJSONPretty = BSL.putStrLn . encodePretty' (defConfig {confIndent = Spaces 2})
