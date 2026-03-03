{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Projection
  ( Customer (..),
    CustomerEvent (..),
    customerProjection,
  )
where

import Bank.Models.Customer.Events
import Data.Aeson.TH
import Eventium
import SumTypesX.TH

newtype Customer
  = Customer
  { name :: Maybe String
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''Customer

constructSumType "CustomerEvent" (defaultSumTypeOptions {sumTypeOptionsTagOptions = AppendTypeNameToTags}) customerEvents

deriving instance Show CustomerEvent

deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent customer (CustomerCreatedCustomerEvent (CustomerCreated n)) = customer {name = Just n}
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing) handleCustomerEvent
