{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.Events
  ( accountEvents,
    AccountOpened (..),
    AccountCredited (..),
    AccountDebited (..),
    AccountTransferStarted (..),
    AccountTransferCompleted (..),
    AccountTransferFailed (..),
    AccountCreditedFromTransfer (..),
  )
where

import Bank.Json
import Eventium (UUID)
import Language.Haskell.TH (Name)

accountEvents :: [Name]
accountEvents =
  [ ''AccountOpened,
    ''AccountCredited,
    ''AccountDebited,
    ''AccountTransferStarted,
    ''AccountTransferCompleted,
    ''AccountTransferFailed,
    ''AccountCreditedFromTransfer
  ]

data AccountOpened
  = AccountOpened
  { accountOpenedOwner :: UUID,
    accountOpenedInitialFunding :: Double
  }
  deriving (Show, Eq)

data AccountCredited
  = AccountCredited
  { accountCreditedAmount :: Double,
    accountCreditedReason :: String
  }
  deriving (Show, Eq)

data AccountDebited
  = AccountDebited
  { accountDebitedAmount :: Double,
    accountDebitedReason :: String
  }
  deriving (Show, Eq)

data AccountTransferStarted
  = AccountTransferStarted
  { accountTransferStartedTransferId :: UUID,
    accountTransferStartedAmount :: Double,
    accountTransferStartedTargetAccount :: UUID
  }
  deriving (Show, Eq)

newtype AccountTransferCompleted
  = AccountTransferCompleted
  { accountTransferCompletedTransferId :: UUID
  }
  deriving (Show, Eq)

data AccountTransferFailed
  = AccountTransferFailed
  { accountTransferFailedTransferId :: UUID,
    accountTransferFailedReason :: String
  }
  deriving (Show, Eq)

data AccountCreditedFromTransfer
  = AccountCreditedFromTransfer
  { accountCreditedFromTransferTransferId :: UUID,
    accountCreditedFromTransferSourceAccount :: UUID,
    accountCreditedFromTransferAmount :: Double
  }
  deriving (Show, Eq)

deriveJSONUnPrefixLower ''AccountOpened
deriveJSONUnPrefixLower ''AccountCredited
deriveJSONUnPrefixLower ''AccountDebited
deriveJSONUnPrefixLower ''AccountTransferStarted
deriveJSONUnPrefixLower ''AccountTransferCompleted
deriveJSONUnPrefixLower ''AccountTransferFailed
deriveJSONUnPrefixLower ''AccountCreditedFromTransfer
