module Cafe.ChefTodoList
  ( chefTodoListMain,
  )
where

import Cafe.CLI.Options (parseDatabaseFileOption)
import Cafe.CLI.Transformer
import Cafe.Models.Tab
import Control.Monad (forM_, unless)
import Control.Monad.Logger (runNoLoggingT)
import Data.IORef
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Eventium
import Eventium.Store.Sqlite
import Options.Applicative
import System.Console.ANSI (clearScreen, setCursorPosition)

-- | Create an in-memory read model that polls the SQLite event store and
-- updates the chef's todo list.
chefTodoListMain :: IO ()
chefTodoListMain = do
  dbFilePath <- execParser $ info (helper <*> parseDatabaseFileOption) (fullDesc <> progDesc "Chef Todo List Terminal")
  pool <- runNoLoggingT $ createSqlitePool (pack dbFilePath) 1

  seqRef <- newIORef (0 :: SequenceNumber)
  foodMapRef <- newIORef (Map.empty :: Map UUID [Maybe Food])

  let checkpoint =
        CheckpointStore
          (readIORef seqRef)
          (writeIORef seqRef)
      globalReader = runEventStoreReaderUsing (`runSqlPool` pool) cliGloballyOrderedEventStore
      sub = pollingSubscription globalReader checkpoint 1000
      handler = EventHandler $ \globalEvent -> do
        let inner = streamEventEvent globalEvent
        case traverse (decode jsonStringCodec) inner of
          Nothing -> return ()
          Just tabEvent -> do
            foodMap <- readIORef foodMapRef
            let foodMap' = handleEventToMap foodMap tabEvent
            writeIORef foodMapRef foodMap'
            printFood foodMap'

  runSubscription sub handler

handleEventToMap :: Map UUID [Maybe Food] -> VersionedStreamEvent TabEvent -> Map UUID [Maybe Food]
handleEventToMap foodMap (StreamEvent uuid _ _ (TabClosed _)) = Map.delete uuid foodMap
handleEventToMap foodMap streamEvent =
  let uuid = streamEventKey streamEvent
      oldList = Map.findWithDefault [] uuid foodMap
   in Map.insert uuid (handleEventToFood oldList $ streamEventEvent streamEvent) foodMap

handleEventToFood :: [Maybe Food] -> TabEvent -> [Maybe Food]
handleEventToFood oldFood (FoodOrdered newFood) = oldFood ++ map Just newFood
handleEventToFood oldFood (FoodPrepared indexes) = setIndexesToNothing indexes oldFood
handleEventToFood food _ = food

printFood :: Map UUID [Maybe Food] -> IO ()
printFood foodMap = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Chef's Todo List:"

  forM_ (Map.keys foodMap) $ \uuid -> do
    let foodItems = catMaybes $ foodMap Map.! uuid
    unless (null foodItems) $ do
      putStrLn $ "Tab: " ++ show uuid
      forM_ foodItems $ \(Food (MenuItem desc _)) -> putStrLn $ "  - Item: " ++ desc
