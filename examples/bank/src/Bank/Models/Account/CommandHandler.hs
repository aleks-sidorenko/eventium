{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Account.CommandHandler
  ( accountCommandHandler,
    AccountCommand (..),
    AccountCommandError (..),
  )
where

import Bank.Models.Account.Commands
import Bank.Models.Account.Events
import Bank.Models.Account.Projection
import Control.Lens
import Data.Maybe (isNothing)
import Eventium
import SumTypesX.TH

constructSumType "AccountCommand" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) accountCommands

-- | Errors returned by the account command handler when a command is rejected.
data AccountCommandError
  = AccountAlreadyOpen
  | InvalidInitialDeposit
  | InsufficientFunds Double
  | AccountNotOpen
  deriving (Show, Eq)

handleAccountCommand :: Account -> AccountCommand -> Either AccountCommandError [AccountEvent]
handleAccountCommand account (OpenAccountAccountCommand OpenAccount {..}) =
  case account ^. accountOwner of
    Just _ -> Left AccountAlreadyOpen
    Nothing ->
      if openAccountInitialFunding < 0
        then Left InvalidInitialDeposit
        else
          Right
            [ AccountOpenedAccountEvent
                AccountOpened
                  { accountOpenedOwner = openAccountOwner,
                    accountOpenedInitialFunding = openAccountInitialFunding
                  }
            ]
handleAccountCommand _ (CreditAccountAccountCommand CreditAccount {..}) =
  Right
    [ AccountCreditedAccountEvent
        AccountCredited
          { accountCreditedAmount = creditAccountAmount,
            accountCreditedReason = creditAccountReason
          }
    ]
handleAccountCommand account (DebitAccountAccountCommand DebitAccount {..}) =
  if accountAvailableBalance account - debitAccountAmount < 0
    then Left $ InsufficientFunds $ accountAvailableBalance account
    else
      Right
        [ AccountDebitedAccountEvent
            AccountDebited
              { accountDebitedAmount = debitAccountAmount,
                accountDebitedReason = debitAccountReason
              }
        ]
handleAccountCommand account (TransferToAccountAccountCommand TransferToAccount {..})
  | isNothing (account ^. accountOwner) =
      Left AccountNotOpen
  | accountAvailableBalance account - transferToAccountAmount < 0 =
      Left $ InsufficientFunds $ accountAvailableBalance account
  | otherwise =
      Right
        [ AccountTransferStartedAccountEvent
            AccountTransferStarted
              { accountTransferStartedTransferId = transferToAccountTransferId,
                accountTransferStartedAmount = transferToAccountAmount,
                accountTransferStartedTargetAccount = transferToAccountTargetAccount
              }
        ]
handleAccountCommand _ (AcceptTransferAccountCommand AcceptTransfer {..}) =
  Right
    [ AccountCreditedFromTransferAccountEvent
        AccountCreditedFromTransfer
          { accountCreditedFromTransferTransferId = acceptTransferTransferId,
            accountCreditedFromTransferSourceAccount = acceptTransferSourceAccount,
            accountCreditedFromTransferAmount = acceptTransferAmount
          }
    ]
handleAccountCommand _ (CompleteTransferAccountCommand CompleteTransfer {..}) =
  Right
    [ AccountTransferCompletedAccountEvent
        AccountTransferCompleted
          { accountTransferCompletedTransferId = completeTransferTransferId
          }
    ]
handleAccountCommand _ (RejectTransferAccountCommand RejectTransfer {..}) =
  Right
    [ AccountTransferFailedAccountEvent
        AccountTransferFailed
          { accountTransferFailedTransferId = rejectTransferTransferId,
            accountTransferFailedReason = rejectTransferReason
          }
    ]

accountCommandHandler :: CommandHandler Account AccountEvent AccountCommand AccountCommandError
accountCommandHandler = CommandHandler handleAccountCommand accountProjection
