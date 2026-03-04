module Bank.CLI.RunCommand
  ( runCLICommand,
  )
where

import Bank.CLI.Options
import Bank.CLI.Store
import Bank.Models
import Bank.ReadModels.CustomerAccounts
import Control.Monad (void)
import Database.Persist.Sqlite
import Eventium

runCLICommand :: ConnectionPool -> CLICommand -> IO ()
runCLICommand pool (CreateCustomerCLI createCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to create customer with UUID: "
  print uuid
  let cmd = CreateCustomerCommand createCommand
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader customerBankCommandHandler uuid cmd
runCLICommand pool (ViewAccountCLI uuid) = do
  latestStreamProj <-
    runDB pool $
      getLatestStreamProjection cliEventStoreReader (versionedStreamProjection uuid accountBankProjection)
  printJSONPretty latestStreamProj.state
runCLICommand pool (ViewCustomerAccountsCLI customerName) = do
  events <- runDB pool $ cliGlobalEventStoreReader.getEvents (allEvents ())
  let allCustomerAccounts = latestProjection customerAccountsProjection ((.payload) <$> events)
      thisCustomerAccounts = getCustomerAccountsFromName allCustomerAccounts customerName
  case thisCustomerAccounts of
    [] -> putStrLn "No accounts found"
    accounts -> mapM_ printJSONPretty accounts
runCLICommand pool (OpenAccountCLI openCommand) = do
  uuid <- uuidNextRandom
  putStr "Attempting to open account with UUID: "
  print uuid
  let cmd = OpenAccountCommand openCommand
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader accountBankCommandHandler uuid cmd
runCLICommand pool (TransferToAccountCLI sourceId transferAmount targetId) = do
  putStrLn $ "Starting transfer from acccount " ++ show sourceId ++ " to " ++ show targetId

  transferId' <- uuidNextRandom
  let startCommand = TransferToAccountCommand $ TransferToAccount transferId' transferAmount targetId
  void $ runDB pool $ applyCommandHandler cliEventStoreWriter cliEventStoreReader accountBankCommandHandler sourceId startCommand
  runCLICommand pool (ViewAccountCLI sourceId)
  runCLICommand pool (ViewAccountCLI targetId)
