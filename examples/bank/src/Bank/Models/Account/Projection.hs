{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.Projection
  ( Account (..),
    accountBalance,
    accountOwner,
    accountPendingTransfers,
    accountAvailableBalance,
    PendingAccountTransfer (..),
    AccountEvent (..),
    accountProjection,
    findAccountTransferById,
  )
where

import Bank.Json
import Bank.Models.Account.Events
import Control.Lens
import Data.Aeson.TH
import Data.List (delete, find)
import Eventium
import SumTypesX.TH

data Account
  = Account
  { _accountBalance :: Double,
    _accountOwner :: Maybe UUID,
    _accountPendingTransfers :: [PendingAccountTransfer]
  }
  deriving (Show, Eq)

accountDefault :: Account
accountDefault = Account 0 Nothing []

data PendingAccountTransfer
  = PendingAccountTransfer
  { pendingAccountTransferId :: UUID,
    pendingAccountTransferAmount :: Double,
    pendingAccountTransferTargetAccount :: UUID
  }
  deriving (Show, Eq)

deriveJSON (unPrefixLower "pendingAccountTransfer") ''PendingAccountTransfer

makeLenses ''Account
deriveJSON (unPrefixLower "_account") ''Account

-- | Account balance minus pending balance
accountAvailableBalance :: Account -> Double
accountAvailableBalance account = account ^. accountBalance - pendingBalance
  where
    transfers = account ^. accountPendingTransfers
    pendingBalance = if null transfers then 0 else sum (map pendingAccountTransferAmount transfers)

findAccountTransferById :: [PendingAccountTransfer] -> UUID -> Maybe PendingAccountTransfer
findAccountTransferById transfers transferId = find ((== transferId) . pendingAccountTransferId) transfers

constructSumType "AccountEvent" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) accountEvents

deriving instance Show AccountEvent

deriving instance Eq AccountEvent

handleAccountEvent :: Account -> AccountEvent -> Account
handleAccountEvent account (AccountOpenedAccountEvent AccountOpened {..}) =
  account
    & accountOwner ?~ accountOpenedOwner
    & accountBalance .~ accountOpenedInitialFunding
handleAccountEvent account (AccountCreditedAccountEvent AccountCredited {..}) =
  account
    & accountBalance +~ accountCreditedAmount
handleAccountEvent account (AccountDebitedAccountEvent AccountDebited {..}) =
  account
    & accountBalance -~ accountDebitedAmount
handleAccountEvent account (AccountTransferStartedAccountEvent AccountTransferStarted {..}) =
  account
    & accountPendingTransfers
      %~ cons
        PendingAccountTransfer
          { pendingAccountTransferId = accountTransferStartedTransferId,
            pendingAccountTransferAmount = accountTransferStartedAmount,
            pendingAccountTransferTargetAccount = accountTransferStartedTargetAccount
          }
handleAccountEvent account (AccountTransferCompletedAccountEvent AccountTransferCompleted {..}) =
  -- If the transfer isn't present, something is wrong, but we can't fail in an
  -- event handler.
  maybe account go (findAccountTransferById transfers accountTransferCompletedTransferId)
  where
    transfers = account ^. accountPendingTransfers
    go trans =
      account
        & accountBalance -~ pendingAccountTransferAmount trans
        & accountPendingTransfers %~ delete trans
handleAccountEvent account (AccountTransferFailedAccountEvent AccountTransferFailed {..}) =
  account
    & accountPendingTransfers .~ transfers'
  where
    transfers = account ^. accountPendingTransfers
    mTransfer = findAccountTransferById transfers accountTransferFailedTransferId
    transfers' = maybe transfers (`delete` transfers) mTransfer
handleAccountEvent account (AccountCreditedFromTransferAccountEvent AccountCreditedFromTransfer {..}) =
  account
    & accountBalance +~ accountCreditedFromTransferAmount

accountProjection :: Projection Account AccountEvent
accountProjection = Projection accountDefault handleAccountEvent
