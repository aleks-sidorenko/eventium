{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventium.JsonSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Eventium.Json (dropPrefix, dropSuffix, unPrefixLower)
import Eventium.Json.TH (deriveJSONUnPrefixLower)
import GHC.Generics (Generic)
import Test.Hspec

data SampleRecord = SampleRecord
  { sampleRecordName :: String,
    sampleRecordAge :: Int
  }
  deriving (Show, Eq, Generic)

deriveJSONUnPrefixLower ''SampleRecord

data AnotherType = AnotherType
  { anotherTypeFoo :: Bool,
    anotherTypeBar :: String
  }
  deriving (Show, Eq, Generic)

deriveJSONUnPrefixLower ''AnotherType

spec :: Spec
spec = describe "Eventium.Json" $ do
  describe "unPrefixLower" $ do
    let opts = unPrefixLower "sampleRecord"
    it "strips prefix and lowercases first char" $ do
      fieldLabelModifier opts "sampleRecordName" `shouldBe` "name"
      fieldLabelModifier opts "sampleRecordAge" `shouldBe` "age"

  describe "dropPrefix" $ do
    it "strips an exact prefix" $ do
      dropPrefix "foo" "fooBar" `shouldBe` "Bar"
      dropPrefix "sample" "sampleRecord" `shouldBe` "Record"

    it "returns remainder when prefix matches exactly" $ do
      dropPrefix "abc" "abcdef" `shouldBe` "def"

  describe "dropSuffix" $ do
    it "strips an exact suffix" $ do
      dropSuffix "Bar" "fooBar" `shouldBe` "foo"

  describe "deriveJSONUnPrefixLower" $ do
    it "produces JSON with stripped and lowercased field names" $ do
      let sample = SampleRecord "Alice" 30
          Object obj = toJSON sample
      KM.lookup "name" obj `shouldBe` Just (String "Alice")
      KM.lookup "age" obj `shouldBe` Just (Number 30)

    it "round-trips through JSON" $ do
      let sample = SampleRecord "Bob" 25
      decode (encode sample) `shouldBe` Just sample

    it "works for different type prefixes" $ do
      let val = AnotherType True "baz"
          Object obj = toJSON val
      KM.lookup "foo" obj `shouldBe` Just (Bool True)
      KM.lookup "bar" obj `shouldBe` Just (String "baz")

    it "round-trips for different types" $ do
      let val = AnotherType False "qux"
      decode (encode val) `shouldBe` Just val
