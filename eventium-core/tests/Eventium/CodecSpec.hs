{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventium.CodecSpec (spec) where

import Data.Dynamic
import Data.Typeable (typeOf)
import Eventium.Codec
import Eventium.TH.SumTypeCodec
import Eventium.TypeEmbedding
import GHC.Generics
import Test.Hspec

data EventA = EventA deriving (Show, Eq)

data EventB = EventB deriving (Show, Eq)

data EventC = EventC deriving (Show, Eq)

data AllEvents
  = AllEventsEventA EventA
  | AllEventsEventB EventB
  | AllEventsEventC EventC
  deriving (Show, Eq, Generic)

instance EventSumType AllEvents

data MyEvents
  = MyEventsEventA EventA
  | MyEventsEventB EventB
  deriving (Show, Eq, Generic)

instance EventSumType MyEvents

mkSumTypeCodec "myEventsCodec" ''MyEvents ''AllEvents

-- Separate subset type for TypeEmbedding tests to avoid TH name clashes
-- (mkSumTypeCodec and mkSumTypeEmbedding generate the same helper
-- function names for the same type pair)
data SubsetEvents
  = SubsetEventsEventA EventA
  | SubsetEventsEventB EventB
  deriving (Show, Eq)

mkSumTypeEmbedding "subsetEventsEmbedding" ''SubsetEvents ''AllEvents

spec :: Spec
spec = do
  describe "EventSumType" $ do
    it "can serialize events without the constructor" $ do
      dynTypeRep (eventToDyn $ MyEventsEventA EventA) `shouldBe` typeOf EventA
      dynTypeRep (eventToDyn $ MyEventsEventA EventA) `shouldBe` dynTypeRep (eventToDyn $ AllEventsEventA EventA)

    it "can deserialize events with the constructor" $ do
      eventFromDyn (toDyn EventA) `shouldBe` Just (MyEventsEventA EventA)
      eventFromDyn (toDyn EventB) `shouldBe` Just (AllEventsEventB EventB)

      eventFromDyn (eventToDyn $ MyEventsEventA EventA) `shouldBe` Just (AllEventsEventA EventA)
      eventFromDyn (eventToDyn $ AllEventsEventB EventB) `shouldBe` Just (MyEventsEventB EventB)

  describe "mkSumTypeCodec" $ do
    it "can serialize events" $ do
      myEventsCodec.encode (MyEventsEventA EventA) `shouldBe` AllEventsEventA EventA
      myEventsCodec.encode (MyEventsEventB EventB) `shouldBe` AllEventsEventB EventB

    it "can deserialize events" $ do
      myEventsCodec.decode (AllEventsEventA EventA) `shouldBe` Just (MyEventsEventA EventA)
      myEventsCodec.decode (AllEventsEventB EventB) `shouldBe` Just (MyEventsEventB EventB)
      myEventsCodec.decode (AllEventsEventC EventC) `shouldBe` Nothing

  describe "mkSumTypeEmbedding" $ do
    it "can embed events" $ do
      subsetEventsEmbedding.embed (SubsetEventsEventA EventA) `shouldBe` AllEventsEventA EventA
      subsetEventsEmbedding.embed (SubsetEventsEventB EventB) `shouldBe` AllEventsEventB EventB

    it "can extract matching events" $ do
      subsetEventsEmbedding.extract (AllEventsEventA EventA) `shouldBe` Just (SubsetEventsEventA EventA)
      subsetEventsEmbedding.extract (AllEventsEventB EventB) `shouldBe` Just (SubsetEventsEventB EventB)

    it "returns Nothing for non-matching events" $ do
      subsetEventsEmbedding.extract (AllEventsEventC EventC) `shouldBe` Nothing
