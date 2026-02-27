-- | Template Haskell helpers for JSON serialization. Separated from
-- "Eventium.Json" to avoid TH linker issues in downstream packages.
module Eventium.Json.TH
  ( deriveJSONUnPrefixLower,
  )
where

import Data.Aeson.TH
import Data.Char (toLower)
import Eventium.Json (unPrefixLower)
import Language.Haskell.TH

-- | Derive 'ToJSON' and 'FromJSON' using 'unPrefixLower' with the type name
-- (first character lowercased) as prefix.
deriveJSONUnPrefixLower :: Name -> Q [Dec]
deriveJSONUnPrefixLower name = deriveJSON (unPrefixLower $ firstCharToLower $ nameBase name) name

firstCharToLower :: String -> String
firstCharToLower [] = []
firstCharToLower (x : xs) = toLower x : xs
