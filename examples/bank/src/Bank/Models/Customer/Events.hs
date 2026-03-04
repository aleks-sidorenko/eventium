{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Events
  ( customerEvents,
    CustomerCreated (..),
    CustomerCreationRejected (..),
  )
where

import Data.Aeson.TH
import Language.Haskell.TH (Name)

customerEvents :: [Name]
customerEvents =
  [ ''CustomerCreated,
    ''CustomerCreationRejected
  ]

newtype CustomerCreated
  = CustomerCreated
  { name :: String
  }
  deriving (Show, Eq)

newtype CustomerCreationRejected
  = CustomerCreationRejected
  { reason :: String
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''CustomerCreated
deriveJSON defaultOptions ''CustomerCreationRejected
