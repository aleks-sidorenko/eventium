{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.MetadataSpec (spec) where

import Data.Aeson (Value (Null, Object), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Eventium.Store.Types (EventMetadata, emptyMetadata, insertCustomMetadata)
import Test.Hspec

spec :: Spec
spec = describe "EventMetadata JSON" $ do
  it "round-trips a non-empty custom map and includes the key" $ do
    let md = insertCustomMetadata "userId" "u-1" (emptyMetadata "Foo")
    decode (encode md) `shouldBe` Just md
    case decode (encode md) :: Maybe Value of
      Just (Object o) -> KM.member "custom" o `shouldBe` True
      _ -> expectationFailure "expected object"

  it "omits custom when empty" $ do
    case decode (encode (emptyMetadata "Foo")) :: Maybe Value of
      Just (Object o) -> KM.member "custom" o `shouldBe` False
      _ -> expectationFailure "expected object"

  it "omits Nothing Maybe fields (no explicit null)" $ do
    case decode (encode (emptyMetadata "Foo")) :: Maybe Value of
      Just (Object o) -> KM.member "correlationId" o `shouldBe` False
      _ -> expectationFailure "expected object"

  it "decodes a legacy row with explicit nulls and no custom key" $ do
    let legacy =
          object
            [ "eventType" .= ("Foo" :: String),
              "correlationId" .= Null,
              "causationId" .= Null,
              "createdAt" .= Null
            ]
    (decode (encode legacy) :: Maybe EventMetadata) `shouldBe` Just (emptyMetadata "Foo")

  it "decodes a new row that omits the optional keys to the same value" $ do
    let new = object ["eventType" .= ("Foo" :: String)]
    (decode (encode new) :: Maybe EventMetadata) `shouldBe` Just (emptyMetadata "Foo")
