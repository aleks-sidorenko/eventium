{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.ProcessManagers.TransferManager
  ( TransferManager (..),
    TransferManagerTransferData (..),
    TransferProcessManager,
    transferProcessManager,
  )
where

import Bank.Models
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Eventium

newtype TransferManager
  = TransferManager
  { _transferManagerData :: Map UUID TransferManagerTransferData
  }
  deriving (Show)

data TransferManagerTransferData
  = TransferManagerTransferData
  { transferSourceAccount :: UUID,
    transferTargetAccount :: UUID
  }
  deriving (Show, Eq)

makeLenses ''TransferManager

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Map.empty

transferManagerProjection :: Projection TransferManager (VersionedStreamEvent BankEvent)
transferManagerProjection =
  Projection
    transferManagerDefault
    handleTransferEvent

handleTransferEvent :: TransferManager -> VersionedStreamEvent BankEvent -> TransferManager
handleTransferEvent manager (StreamEvent sourceAccount _ _ (AccountTransferStartedEvent AccountTransferStarted {..})) =
  manager
    & transferManagerData . at accountTransferStartedTransferId
      ?~ TransferManagerTransferData
        { transferSourceAccount = sourceAccount,
          transferTargetAccount = accountTransferStartedTargetAccount
        }
handleTransferEvent manager _ = manager

reactTransfer :: TransferManager -> VersionedStreamEvent BankEvent -> [ProcessManagerEffect BankCommand]
reactTransfer manager (StreamEvent sourceAccount _ _ (AccountTransferStartedEvent AccountTransferStarted {..}))
  | isNothing (manager ^. transferManagerData . at accountTransferStartedTransferId) =
      [ IssueCommand
          accountTransferStartedTargetAccount
          ( AcceptTransferCommand
              AcceptTransfer
                { acceptTransferTransferId = accountTransferStartedTransferId,
                  acceptTransferSourceAccount = sourceAccount,
                  acceptTransferAmount = accountTransferStartedAmount
                }
          )
      ]
  | otherwise = []
reactTransfer manager (StreamEvent _ _ _ (AccountCreditedFromTransferEvent AccountCreditedFromTransfer {..})) =
  maybe [] mkEffect (manager ^. transferManagerData . at accountCreditedFromTransferTransferId)
  where
    mkEffect (TransferManagerTransferData sourceId _) =
      [ IssueCommand
          sourceId
          ( CompleteTransferCommand $
              CompleteTransfer accountCreditedFromTransferTransferId
          )
      ]
reactTransfer _ _ = []

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
    transferManagerProjection
    reactTransfer
