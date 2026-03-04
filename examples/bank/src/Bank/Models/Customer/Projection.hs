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
import Eventium.TH.SumType

newtype Customer
  = Customer
  { name :: Maybe String
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''Customer

constructSumType "CustomerEvent" (withTagOptions AppendTypeNameToTags defaultSumTypeOptions) customerEvents

deriving instance Show CustomerEvent

deriving instance Eq CustomerEvent

handleCustomerEvent :: Customer -> CustomerEvent -> Customer
handleCustomerEvent _customer (CustomerCreatedCustomerEvent (CustomerCreated n)) = Customer {name = Just n}
handleCustomerEvent customer (CustomerCreationRejectedCustomerEvent _) = customer

customerProjection :: Projection Customer CustomerEvent
customerProjection = Projection (Customer Nothing) handleCustomerEvent
