-- | Re-exports JSON serialization utilities from Eventium.
module Bank.Json
  ( unPrefixLower,
    dropPrefix,
    dropSuffix,
    deriveJSONUnPrefixLower,
  )
where

import Eventium.Json
import Eventium.Json.TH
