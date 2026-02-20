{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models
  ( BankEvent (..),
    BankCommand (..),
    accountEventEmbedding,
    accountCommandEmbedding,
    accountBankProjection,
    accountBankCommandHandler,
    customerEventEmbedding,
    customerCommandEmbedding,
    customerBankProjection,
    customerBankCommandHandler,
    module X,
  )
where

import Bank.Json
import Bank.Models.Account as X
import Bank.Models.Customer as X
import Data.Aeson.TH
import Data.Void (Void)
import Eventium
import Eventium.TH
import SumTypesX.TH

constructSumType "BankEvent" (defaultSumTypeOptions {sumTypeOptionsTagOptions = ConstructTagName (++ "Event")}) $
  accountEvents ++ customerEvents

deriving instance Show BankEvent

deriving instance Eq BankEvent

deriveJSON (defaultOptions {constructorTagModifier = dropSuffix "Event"}) ''BankEvent

constructSumType "BankCommand" (defaultSumTypeOptions {sumTypeOptionsTagOptions = ConstructTagName (++ "Command")}) $
  accountCommands ++ customerCommands

deriving instance Show BankCommand

deriving instance Eq BankCommand

mkSumTypeEmbedding "accountEventEmbedding" ''AccountEvent ''BankEvent
mkSumTypeEmbedding "accountCommandEmbedding" ''AccountCommand ''BankCommand

accountBankProjection :: Projection Account BankEvent
accountBankProjection = embeddedProjection accountEventEmbedding accountProjection

accountBankCommandHandler :: CommandHandler Account BankEvent BankCommand AccountCommandError
accountBankCommandHandler = embeddedCommandHandler accountEventEmbedding accountCommandEmbedding accountCommandHandler

mkSumTypeEmbedding "customerEventEmbedding" ''CustomerEvent ''BankEvent
mkSumTypeEmbedding "customerCommandEmbedding" ''CustomerCommand ''BankCommand

customerBankProjection :: Projection Customer BankEvent
customerBankProjection = embeddedProjection customerEventEmbedding customerProjection

customerBankCommandHandler :: CommandHandler Customer BankEvent BankCommand Void
customerBankCommandHandler = embeddedCommandHandler customerEventEmbedding customerCommandEmbedding customerCommandHandler
