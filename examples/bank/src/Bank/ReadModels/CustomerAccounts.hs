{-# LANGUAGE TupleSections #-}

module Bank.ReadModels.CustomerAccounts
  ( CustomerAccounts (..),
    getCustomerAccountsFromName,
    customerAccountsProjection,
  )
where

import Bank.Models
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Eventium

-- | Groups account info by customer so it's easy to see all of a customer's
-- accounts
data CustomerAccounts
  = CustomerAccounts
  { accountsById :: Map UUID Account,
    customerAccounts :: Map UUID [UUID],
    customerIdsByName :: Map String UUID
    -- NOTE: This assumes all customer names are unique. Obviously not true in
    -- the real world.
  }
  deriving (Show, Eq)

getCustomerAccountsFromName :: CustomerAccounts -> String -> [(UUID, Account)]
getCustomerAccountsFromName ca customerName = fromMaybe [] $ do
  customerId <- Map.lookup customerName ca.customerIdsByName
  accountIds <- Map.lookup customerId ca.customerAccounts
  let lookupAccount uuid = (uuid,) <$> Map.lookup uuid ca.accountsById
  return $ mapMaybe lookupAccount accountIds

handleCustomerAccountsEvent :: CustomerAccounts -> VersionedStreamEvent BankEvent -> CustomerAccounts
handleCustomerAccountsEvent accounts (StreamEvent uuid _ _ (CustomerCreatedEvent (CustomerCreated customerName))) =
  accounts {customerIdsByName = Map.insert customerName uuid accounts.customerIdsByName}
handleCustomerAccountsEvent accounts (StreamEvent uuid _ _ (AccountOpenedEvent event@(AccountOpened customerId _))) =
  accounts
    { accountsById = Map.insert uuid account accounts.accountsById,
      customerAccounts = Map.insertWith (++) customerId [uuid] accounts.customerAccounts
    }
  where
    account = accountProjection.eventHandler accountProjection.seed (AccountOpenedAccountEvent event)
-- Assume it's an account event. If it isn't it won't get handled, no biggy.
handleCustomerAccountsEvent accounts (StreamEvent uuid _ _ event) =
  accounts {accountsById = Map.adjust modifyAccount uuid accounts.accountsById}
  where
    modifyAccount account =
      maybe account (accountProjection.eventHandler account) (accountEventEmbedding.extract event)

customerAccountsProjection :: Projection CustomerAccounts (VersionedStreamEvent BankEvent)
customerAccountsProjection =
  Projection
    (CustomerAccounts Map.empty Map.empty Map.empty)
    handleCustomerAccountsEvent
