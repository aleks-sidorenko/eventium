{-# LANGUAGE TemplateHaskell #-}

module Bank.ProcessManagers.TransferManager
  ( TransferManager (..),
    TransferManagerTransferData (..),
    TransferProcessManager,
    transferProcessManager,
  )
where

import Bank.Models
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Eventium

newtype TransferManager
  = TransferManager
  { transferData :: Map UUID TransferManagerTransferData
  }
  deriving (Show)

data TransferManagerTransferData
  = TransferManagerTransferData
  { sourceAccount :: UUID,
    targetAccount :: UUID
  }
  deriving (Show, Eq)

transferManagerDefault :: TransferManager
transferManagerDefault = TransferManager Map.empty

transferManagerProjection :: Projection TransferManager (VersionedStreamEvent BankEvent)
transferManagerProjection =
  Projection
    transferManagerDefault
    handleTransferEvent

handleTransferEvent :: TransferManager -> VersionedStreamEvent BankEvent -> TransferManager
handleTransferEvent manager (StreamEvent sourceAcct _ _ (AccountTransferStartedEvent evt)) =
  manager
    { transferData =
        Map.insert
          evt.transferId
          TransferManagerTransferData
            { sourceAccount = sourceAcct,
              targetAccount = evt.targetAccount
            }
          manager.transferData
    }
handleTransferEvent manager _ = manager

reactTransfer :: TransferManager -> VersionedStreamEvent BankEvent -> [ProcessManagerEffect BankCommand]
reactTransfer manager (StreamEvent sourceAcct _ _ (AccountTransferStartedEvent evt))
  | isNothing (Map.lookup evt.transferId manager.transferData) =
      [ IssueCommandWithCompensation
          evt.targetAccount
          ( AcceptTransferCommand
              AcceptTransfer
                { transferId = evt.transferId,
                  sourceAccount = sourceAcct,
                  amount = evt.amount
                }
          )
          ( \(RejectionReason rejectionReason) ->
              [ IssueCommand
                  sourceAcct
                  ( RejectTransferCommand
                      RejectTransfer
                        { transferId = evt.transferId,
                          reason = T.unpack rejectionReason
                        }
                  )
              ]
          )
      ]
  | otherwise = []
reactTransfer manager (StreamEvent _ _ _ (AccountCreditedFromTransferEvent evt)) =
  maybe [] mkEffect (Map.lookup evt.transferId manager.transferData)
  where
    mkEffect (TransferManagerTransferData sourceAcct _) =
      [ IssueCommand
          sourceAcct
          ( CompleteTransferCommand $
              CompleteTransfer evt.transferId
          )
      ]
reactTransfer _ _ = []

type TransferProcessManager = ProcessManager TransferManager BankEvent BankCommand

transferProcessManager :: TransferProcessManager
transferProcessManager =
  ProcessManager
    transferManagerProjection
    reactTransfer
