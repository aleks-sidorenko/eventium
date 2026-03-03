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
handleAccountCommand account (OpenAccountAccountCommand cmd) =
  case account.owner of
    Just _ -> Left AccountAlreadyOpen
    Nothing ->
      if cmd.initialFunding < 0
        then Left InvalidInitialDeposit
        else
          Right
            [ AccountOpenedAccountEvent
                AccountOpened
                  { owner = cmd.owner,
                    initialFunding = cmd.initialFunding
                  }
            ]
handleAccountCommand _ (CreditAccountAccountCommand cmd) =
  Right
    [ AccountCreditedAccountEvent
        AccountCredited
          { amount = cmd.amount,
            reason = cmd.reason
          }
    ]
handleAccountCommand account (DebitAccountAccountCommand cmd) =
  if accountAvailableBalance account - cmd.amount < 0
    then Left $ InsufficientFunds $ accountAvailableBalance account
    else
      Right
        [ AccountDebitedAccountEvent
            AccountDebited
              { amount = cmd.amount,
                reason = cmd.reason
              }
        ]
handleAccountCommand account (TransferToAccountAccountCommand cmd)
  | isNothing account.owner =
      Left AccountNotOpen
  | accountAvailableBalance account - cmd.amount < 0 =
      Left $ InsufficientFunds $ accountAvailableBalance account
  | otherwise =
      Right
        [ AccountTransferStartedAccountEvent
            AccountTransferStarted
              { transferId = cmd.transferId,
                amount = cmd.amount,
                targetAccount = cmd.targetAccount
              }
        ]
handleAccountCommand _ (AcceptTransferAccountCommand cmd) =
  Right
    [ AccountCreditedFromTransferAccountEvent
        AccountCreditedFromTransfer
          { transferId = cmd.transferId,
            sourceAccount = cmd.sourceAccount,
            amount = cmd.amount
          }
    ]
handleAccountCommand _ (CompleteTransferAccountCommand cmd) =
  Right
    [ AccountTransferCompletedAccountEvent
        AccountTransferCompleted
          { transferId = cmd.transferId
          }
    ]
handleAccountCommand _ (RejectTransferAccountCommand cmd) =
  Right
    [ AccountTransferFailedAccountEvent
        AccountTransferFailed
          { transferId = cmd.transferId,
            reason = cmd.reason
          }
    ]

accountCommandHandler :: CommandHandler Account AccountEvent AccountCommand AccountCommandError
accountCommandHandler = CommandHandler handleAccountCommand accountProjection
