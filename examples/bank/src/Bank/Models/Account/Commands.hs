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

import Bank.Json
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
  { openAccountOwner :: UUID,
    openAccountInitialFunding :: Double
  }
  deriving (Show, Eq)

data CreditAccount
  = CreditAccount
  { creditAccountAmount :: Double,
    creditAccountReason :: String
  }
  deriving (Show, Eq)

data DebitAccount
  = DebitAccount
  { debitAccountAmount :: Double,
    debitAccountReason :: String
  }
  deriving (Show, Eq)

data TransferToAccount
  = TransferToAccount
  { transferToAccountTransferId :: UUID,
    transferToAccountAmount :: Double,
    transferToAccountTargetAccount :: UUID
  }
  deriving (Show, Eq)

data AcceptTransfer
  = AcceptTransfer
  { acceptTransferTransferId :: UUID,
    acceptTransferSourceAccount :: UUID,
    acceptTransferAmount :: Double
  }
  deriving (Show, Eq)

newtype CompleteTransfer
  = CompleteTransfer
  { completeTransferTransferId :: UUID
  }
  deriving (Show, Eq)

data RejectTransfer
  = RejectTransfer
  { rejectTransferTransferId :: UUID,
    rejectTransferReason :: String
  }
  deriving (Show, Eq)

deriveJSONUnPrefixLower ''OpenAccount
deriveJSONUnPrefixLower ''CreditAccount
deriveJSONUnPrefixLower ''DebitAccount
deriveJSONUnPrefixLower ''TransferToAccount
deriveJSONUnPrefixLower ''AcceptTransfer
deriveJSONUnPrefixLower ''CompleteTransfer
deriveJSONUnPrefixLower ''RejectTransfer
