{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Eventium.SumTypeSpec (spec) where

import Eventium.TH.SumType
import Test.Hspec

-- Test payload types

data Alpha = Alpha deriving (Show, Eq)

data Beta = Beta deriving (Show, Eq)

data Gamma = Gamma deriving (Show, Eq)

-- 1. constructSumType with default PrefixTagsWithTypeName
-- Generates: data Prefixed = PrefixedAlpha Alpha | PrefixedBeta Beta | PrefixedGamma Gamma
constructSumType "Prefixed" defaultSumTypeOptions [''Alpha, ''Beta, ''Gamma]

deriving instance Show Prefixed

deriving instance Eq Prefixed

-- 2. constructSumType with AppendTypeNameToTags
-- Generates: data Appended = AlphaAppended Alpha | BetaAppended Beta
constructSumType "Appended" (withTagOptions AppendTypeNameToTags defaultSumTypeOptions) [''Alpha, ''Beta]

deriving instance Show Appended

deriving instance Eq Appended

-- 3. constructSumType with custom ConstructTagName
-- Generates: data Custom = AlphaEvent Alpha | BetaEvent Beta
constructSumType "Custom" (withTagOptions (ConstructTagName (++ "Event")) defaultSumTypeOptions) [''Alpha, ''Beta]

deriving instance Show Custom

deriving instance Eq Custom

-- 4. A subset for converter tests (only Alpha, Beta — Prefixed has all three)
constructSumType "Subset" defaultSumTypeOptions [''Alpha, ''Beta]

deriving instance Show Subset

deriving instance Eq Subset

-- 5. sumTypeConverter: total conversion Subset -> Prefixed
sumTypeConverter "subsetToPrefixed" ''Subset ''Prefixed

-- 6. partialSumTypeConverter: partial conversion Prefixed -> Subset
partialSumTypeConverter "prefixedToSubset" ''Prefixed ''Subset

spec :: Spec
spec = do
  describe "constructSumType" $ do
    describe "PrefixTagsWithTypeName (default)" $ do
      it "generates constructors prefixed with the type name" $ do
        PrefixedAlpha Alpha `shouldBe` PrefixedAlpha Alpha
        PrefixedBeta Beta `shouldBe` PrefixedBeta Beta
        PrefixedGamma Gamma `shouldBe` PrefixedGamma Gamma

    describe "AppendTypeNameToTags" $ do
      it "generates constructors with type name appended" $ do
        AlphaAppended Alpha `shouldBe` AlphaAppended Alpha
        BetaAppended Beta `shouldBe` BetaAppended Beta

    describe "ConstructTagName" $ do
      it "generates constructors using the custom naming function" $ do
        AlphaEvent Alpha `shouldBe` AlphaEvent Alpha
        BetaEvent Beta `shouldBe` BetaEvent Beta

  describe "sumTypeConverter" $ do
    it "converts all source constructors to matching target constructors" $ do
      subsetToPrefixed (SubsetAlpha Alpha) `shouldBe` PrefixedAlpha Alpha
      subsetToPrefixed (SubsetBeta Beta) `shouldBe` PrefixedBeta Beta

  describe "partialSumTypeConverter" $ do
    it "returns Just for constructors present in the target type" $ do
      prefixedToSubset (PrefixedAlpha Alpha) `shouldBe` Just (SubsetAlpha Alpha)
      prefixedToSubset (PrefixedBeta Beta) `shouldBe` Just (SubsetBeta Beta)

    it "returns Nothing for constructors not in the target type" $ do
      prefixedToSubset (PrefixedGamma Gamma) `shouldBe` Nothing
