{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.Projection
  ( Account (..),
    accountAvailableBalance,
    PendingAccountTransfer (..),
    AccountEvent (..),
    accountProjection,
    findAccountTransferById,
  )
where

import Bank.Models.Account.Events
import Data.Aeson.TH
import Data.List (delete, find)
import Eventium
import SumTypesX.TH

data Account
  = Account
  { balance :: Double,
    owner :: Maybe UUID,
    pendingTransfers :: [PendingAccountTransfer]
  }
  deriving (Show, Eq)

accountDefault :: Account
accountDefault = Account 0 Nothing []

data PendingAccountTransfer
  = PendingAccountTransfer
  { id :: UUID,
    amount :: Double,
    targetAccount :: UUID
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''PendingAccountTransfer
deriveJSON defaultOptions ''Account

-- | Account balance minus pending balance
accountAvailableBalance :: Account -> Double
accountAvailableBalance account = account.balance - pendingBalance
  where
    transfers = account.pendingTransfers
    pendingBalance = if null transfers then 0 else sum (map (.amount) transfers)

findAccountTransferById :: [PendingAccountTransfer] -> UUID -> Maybe PendingAccountTransfer
findAccountTransferById transfers transferId' = find (\t -> t.id == transferId') transfers

constructSumType "AccountEvent" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) accountEvents

deriving instance Show AccountEvent

deriving instance Eq AccountEvent

handleAccountEvent :: Account -> AccountEvent -> Account
handleAccountEvent account (AccountOpenedAccountEvent evt) =
  account
    { owner = Just evt.owner,
      balance = evt.initialFunding
    }
handleAccountEvent account (AccountCreditedAccountEvent evt) =
  account {balance = account.balance + evt.amount}
handleAccountEvent account (AccountDebitedAccountEvent evt) =
  account {balance = account.balance - evt.amount}
handleAccountEvent account (AccountTransferStartedAccountEvent evt) =
  account
    { pendingTransfers =
        PendingAccountTransfer
          { id = evt.transferId,
            amount = evt.amount,
            targetAccount = evt.targetAccount
          }
          : account.pendingTransfers
    }
handleAccountEvent account (AccountTransferCompletedAccountEvent evt) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (findAccountTransferById transfers evt.transferId)
  where
    transfers = account.pendingTransfers
    go trans =
      account
        { balance = account.balance - trans.amount,
          pendingTransfers = delete trans account.pendingTransfers
        }
handleAccountEvent account (AccountTransferFailedAccountEvent evt) =
  account {pendingTransfers = transfers'}
  where
    transfers = account.pendingTransfers
    mTransfer = findAccountTransferById transfers evt.transferId
    transfers' = maybe transfers (`delete` transfers) mTransfer
handleAccountEvent account (AccountCreditedFromTransferAccountEvent evt) =
  account {balance = account.balance + evt.amount}

accountProjection :: Projection Account AccountEvent
accountProjection = Projection accountDefault handleAccountEvent
