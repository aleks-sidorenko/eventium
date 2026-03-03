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

import Data.Aeson.TH
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
  { owner :: UUID,
    initialFunding :: Double
  }
  deriving (Show, Eq)

data AccountCredited
  = AccountCredited
  { amount :: Double,
    reason :: String
  }
  deriving (Show, Eq)

data AccountDebited
  = AccountDebited
  { amount :: Double,
    reason :: String
  }
  deriving (Show, Eq)

data AccountTransferStarted
  = AccountTransferStarted
  { transferId :: UUID,
    amount :: Double,
    targetAccount :: UUID
  }
  deriving (Show, Eq)

newtype AccountTransferCompleted
  = AccountTransferCompleted
  { transferId :: UUID
  }
  deriving (Show, Eq)

data AccountTransferFailed
  = AccountTransferFailed
  { transferId :: UUID,
    reason :: String
  }
  deriving (Show, Eq)

data AccountCreditedFromTransfer
  = AccountCreditedFromTransfer
  { transferId :: UUID,
    sourceAccount :: UUID,
    amount :: Double
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''AccountOpened
deriveJSON defaultOptions ''AccountCredited
deriveJSON defaultOptions ''AccountDebited
deriveJSON defaultOptions ''AccountTransferStarted
deriveJSON defaultOptions ''AccountTransferCompleted
deriveJSON defaultOptions ''AccountTransferFailed
deriveJSON defaultOptions ''AccountCreditedFromTransfer
