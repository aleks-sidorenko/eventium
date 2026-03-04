{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.CommandHandler
  ( CustomerCommand (..),
    customerCommandHandler,
  )
where

import Bank.Models.Customer.Commands
import Bank.Models.Customer.Events
import Bank.Models.Customer.Projection
import Data.Void (Void)
import Eventium
import SumTypesX.TH

constructSumType "CustomerCommand" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) customerCommands

handleCustomerCommand :: Customer -> CustomerCommand -> Either Void [CustomerEvent]
handleCustomerCommand customer (CreateCustomerCustomerCommand (CreateCustomer n)) =
  Right $
    case customer.name of
      Nothing -> [CustomerCreatedCustomerEvent $ CustomerCreated n]
      Just _ -> [CustomerCreationRejectedCustomerEvent $ CustomerCreationRejected "Customer already exists"]

customerCommandHandler :: CommandHandler Customer CustomerEvent CustomerCommand Void
customerCommandHandler = CommandHandler handleCustomerCommand customerProjection
