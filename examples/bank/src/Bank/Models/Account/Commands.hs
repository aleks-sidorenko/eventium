{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.Commands
  ( accountCommands,
    OpenAccount (..),
    CreditAccount (..),
    DebitAccount (..),
    TransferToAccount (..),
    AcceptTransfer (..),
    CompleteTransfer (..),
    RejectTransfer (..),
  )
where

import Data.Aeson.TH
import Eventium.UUID
import Language.Haskell.TH (Name)

accountCommands :: [Name]
accountCommands =
  [ ''OpenAccount,
    ''CreditAccount,
    ''DebitAccount,
    ''TransferToAccount,
    ''AcceptTransfer,
    ''CompleteTransfer,
    ''RejectTransfer
  ]

data OpenAccount
  = OpenAccount
  { owner :: UUID,
    initialFunding :: Double
  }
  deriving (Show, Eq)

data CreditAccount
  = CreditAccount
  { amount :: Double,
    reason :: String
  }
  deriving (Show, Eq)

data DebitAccount
  = DebitAccount
  { amount :: Double,
    reason :: String
  }
  deriving (Show, Eq)

data TransferToAccount
  = TransferToAccount
  { transferId :: UUID,
    amount :: Double,
    targetAccount :: UUID
  }
  deriving (Show, Eq)

data AcceptTransfer
  = AcceptTransfer
  { transferId :: UUID,
    sourceAccount :: UUID,
    amount :: Double
  }
  deriving (Show, Eq)

newtype CompleteTransfer
  = CompleteTransfer
  { transferId :: UUID
  }
  deriving (Show, Eq)

data RejectTransfer
  = RejectTransfer
  { transferId :: UUID,
    reason :: String
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''OpenAccount
deriveJSON defaultOptions ''CreditAccount
deriveJSON defaultOptions ''DebitAccount
deriveJSON defaultOptions ''TransferToAccount
deriveJSON defaultOptions ''AcceptTransfer
deriveJSON defaultOptions ''CompleteTransfer
deriveJSON defaultOptions ''RejectTransfer
