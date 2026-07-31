{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventium.SchemaEvolutionSpec (spec) where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    object,
    toJSON,
    (.=),
  )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Eventium.Codec (Codec (..))
import Eventium.SchemaEvolution
import GHC.Generics (Generic)
import Test.Hspec

-- A v2 shape that adds a boolean field ('enabled') absent from v1 events —
-- the canonical "required field added" migration.
data Gizmo = Gizmo
  { kind :: Text,
    size :: Int,
    enabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Gizmo

instance FromJSON Gizmo

-- A type with a two-hop chain (v1 -> v2 -> v3), for version-skipping.
data Chain = Chain
  { kind :: Text,
    step :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Chain

instance FromJSON Chain

-- A type with no registered upcasters, for the identity path.
data Plain = Plain
  { kind :: Text,
    note :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Plain

instance FromJSON Plain

-- | Read the event-type name from a payload's @kind@ field.
kindOf :: Value -> Maybe EventTypeName
kindOf (Object o) = case KeyMap.lookup "kind" o of
  Just (String t) -> Just t
  _ -> Nothing
kindOf _ = Nothing

-- v1 -> v2 for Gizmo: default the new field to False. Insert-if-absent so the
-- hop is idempotent and never clobbers an existing value.
injectEnabled :: Value -> Value
injectEnabled (Object o) = Object (KeyMap.insertWith (\_new old -> old) "enabled" (Bool False) o)
injectEnabled v = v

setStep :: Int -> Value -> Value
setStep n (Object o) = Object (KeyMap.insert "step" (toJSON n) o)
setStep _ v = v

registry :: SchemaRegistry Value
registry =
  registerUpcasters "Gizmo" [injectEnabled] $
    registerUpcasters "Chain" [setStep 2, setStep 3] emptyRegistry

codec :: Codec Gizmo Value
codec = upcastingValueCodec kindOf registry

chainCodec :: Codec Chain Value
chainCodec = upcastingValueCodec kindOf registry

plainCodec :: Codec Plain Value
plainCodec = upcastingValueCodec kindOf registry

spec :: Spec
spec = do
  describe "SchemaRegistry" $ do
    it "current version is 1 + number of hops" $ do
      currentVersion registry "Gizmo" `shouldBe` 2
      currentVersion registry "Chain" `shouldBe` 3

    it "defaults unregistered types to version 1" $
      currentVersion registry "Unknown" `shouldBe` 1

  describe "envelope" $ do
    it "wraps a payload with its schema version" $
      wrapEnvelope 2 (object ["kind" .= ("Gizmo" :: Text)])
        `shouldBe` object ["schemaVersion" .= (2 :: Int), "payload" .= object ["kind" .= ("Gizmo" :: Text)]]

    it "unwraps an envelope to (version, payload)" $
      unwrapEnvelope (object ["schemaVersion" .= (2 :: Int), "payload" .= object ["kind" .= ("Gizmo" :: Text)]])
        `shouldBe` (2, object ["kind" .= ("Gizmo" :: Text)])

    it "treats legacy bare JSON as version 1" $
      unwrapEnvelope (object ["kind" .= ("Gizmo" :: Text)])
        `shouldBe` (1, object ["kind" .= ("Gizmo" :: Text)])

    it "is not fooled by a payload that merely has a schemaVersion field but no payload sibling" $
      unwrapEnvelope (object ["schemaVersion" .= (7 :: Int), "kind" .= ("Gizmo" :: Text)])
        `shouldBe` (1, object ["schemaVersion" .= (7 :: Int), "kind" .= ("Gizmo" :: Text)])

  describe "upcastingValueCodec encode" $ do
    it "wraps writes in the current-version envelope" $
      codec.encode (Gizmo "Gizmo" 100 True)
        `shouldBe` object
          [ "schemaVersion" .= (2 :: Int),
            "payload" .= object ["kind" .= ("Gizmo" :: Text), "size" .= (100 :: Int), "enabled" .= True]
          ]

  describe "upcastingValueCodec decode" $ do
    it "decodes a legacy v1 event by running the chain (added field defaulted)" $
      codec.decode (object ["kind" .= ("Gizmo" :: Text), "size" .= (100 :: Int)])
        `shouldBe` Just (Gizmo "Gizmo" 100 False)

    it "round-trips a current-version event, preserving the field" $
      codec.decode (codec.encode (Gizmo "Gizmo" 250 True))
        `shouldBe` Just (Gizmo "Gizmo" 250 True)

    it "runs every hop for a legacy v1 event (version-skipping)" $
      chainCodec.decode (object ["kind" .= ("Chain" :: Text), "step" .= (1 :: Int)])
        `shouldBe` Just (Chain "Chain" 3)

    it "runs only the remaining hops for an event stored at an intermediate version" $
      chainCodec.decode (object ["schemaVersion" .= (2 :: Int), "payload" .= object ["kind" .= ("Chain" :: Text), "step" .= (99 :: Int)]])
        `shouldBe` Just (Chain "Chain" 3)

    it "is identity for an unregistered event type" $
      plainCodec.decode (object ["kind" .= ("Plain" :: Text), "note" .= ("hi" :: Text)])
        `shouldBe` Just (Plain "Plain" "hi")

  describe "upcaster combinators" $ do
    it "addFieldIfAbsent inserts a default when the field is missing" $
      addFieldIfAbsent "enabled" (Bool False) (object ["kind" .= ("Gizmo" :: Text)])
        `shouldBe` object ["kind" .= ("Gizmo" :: Text), "enabled" .= False]

    it "addFieldIfAbsent leaves an existing field untouched" $
      addFieldIfAbsent "enabled" (Bool False) (object ["enabled" .= True])
        `shouldBe` object ["enabled" .= True]

    it "removeField drops a field" $
      removeField "legacy" (object ["kind" .= ("Gizmo" :: Text), "legacy" .= (1 :: Int)])
        `shouldBe` object ["kind" .= ("Gizmo" :: Text)]

    it "renameField moves a value to the new key" $
      renameField "amount" "size" (object ["amount" .= (10 :: Int)])
        `shouldBe` object ["size" .= (10 :: Int)]

    it "renameField is a no-op when the source key is absent" $
      renameField "amount" "size" (object ["size" .= (10 :: Int)])
        `shouldBe` object ["size" .= (10 :: Int)]

    it "atKey focuses an upcaster into a nested object" $
      atKey "contents" (addFieldIfAbsent "enabled" (Bool False)) (object ["tag" .= ("G" :: Text), "contents" .= object ["size" .= (1 :: Int)]])
        `shouldBe` object ["tag" .= ("G" :: Text), "contents" .= object ["size" .= (1 :: Int), "enabled" .= False]]

    it "atKey is a no-op when the focused key is absent" $
      atKey "contents" (addFieldIfAbsent "enabled" (Bool False)) (object ["tag" .= ("G" :: Text)])
        `shouldBe` object ["tag" .= ("G" :: Text)]

    it "combinators pass non-object values through unchanged" $
      addFieldIfAbsent "x" (Bool True) (String "scalar") `shouldBe` String "scalar"
