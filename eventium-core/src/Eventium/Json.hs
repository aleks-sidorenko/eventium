-- | Shared JSON serialization utilities. Provides Aeson 'Options' and string
-- helpers that strip record-field prefixes, matching the convention used
-- across all eventium packages and example applications.
module Eventium.Json
  ( unPrefixLower,
    dropPrefix,
    dropSuffix,
  )
where

import Data.Aeson
import Data.Char (toLower)

-- | Aeson 'Options' that strip a given prefix from record field names
-- and lowercase the first character of the remainder.
unPrefixLower :: String -> Options
unPrefixLower prefix =
  defaultOptions
    { fieldLabelModifier = unCapitalize . dropPrefix prefix
    }

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c : cs) = toLower c : cs

-- | Strip an exact prefix from a string. Errors if the prefix is not present.
dropPrefix :: String -> String -> String
dropPrefix = dropPrefix' "dropPrefix" id

-- | Strip an exact suffix from a string. Errors if the suffix is not present.
dropSuffix :: String -> String -> String
dropSuffix prefix input = reverse $ dropPrefix' "dropSuffix" reverse (reverse prefix) (reverse input)

dropPrefix' :: String -> (String -> String) -> String -> String -> String
dropPrefix' fnName strTrans prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " ++ strTrans pre
    go [] (c : cs) = c : cs
    go (p : preRest) (c : cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " ++ strTrans (p : preRest) ++ " " ++ strTrans (c : cRest)
    contextual msg = fnName ++ ": " ++ msg ++ ". " ++ strTrans prefix ++ " " ++ strTrans input
