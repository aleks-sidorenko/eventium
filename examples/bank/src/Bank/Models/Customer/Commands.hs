{-# LANGUAGE TemplateHaskell #-}

module Bank.Models.Customer.Commands
  ( customerCommands,
    CreateCustomer (..),
  )
where

import Data.Aeson.TH
import Language.Haskell.TH (Name)

customerCommands :: [Name]
customerCommands =
  [ ''CreateCustomer
  ]

newtype CreateCustomer
  = CreateCustomer
  { name :: String
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''CreateCustomer
