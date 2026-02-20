{-# LANGUAGE OverloadedStrings #-}

module Eventium.ProjectionSpec (spec) where

import Control.Exception (evaluate)
import Data.Functor.Contravariant
import Eventium.Codec (Codec (..), DecodeError (..))
import Eventium.Projection
import Eventium.Store.Types
import Test.Hspec

-- Simple test projection: counter that sums integers
counterProjection :: Projection Int Int
counterProjection = Projection 0 (+)

spec :: Spec
spec = do
  describe "latestProjection" $ do
    it "should compute the correct final state" $ do
      latestProjection counterProjection [1, 2, 3, 4] `shouldBe` 10

    it "should return the seed for an empty event list" $ do
      latestProjection counterProjection [] `shouldBe` 0

  describe "allProjections" $ do
    it "should return all intermediate states" $ do
      allProjections counterProjection [1, 2, 3] `shouldBe` [0, 1, 3, 6]

    it "should return just the seed for an empty event list" $ do
      allProjections counterProjection [] `shouldBe` [0]

  describe "Contravariant instance" $ do
    it "should adapt the event type via contramap" $ do
      let stringProjection = contramap (length :: String -> Int) counterProjection
      latestProjection stringProjection ["hi", "hello", "a"] `shouldBe` 8

  describe "projectionMapMaybe" $ do
    it "should filter events and apply matching ones" $ do
      let evenOnly n = if even n then Just n else Nothing
          evenProjection = projectionMapMaybe evenOnly counterProjection
      latestProjection evenProjection [1, 2, 3, 4, 5, 6] `shouldBe` 12

    it "should return seed when no events match" $ do
      let noneMatch _ = Nothing
          emptyProjection = projectionMapMaybe noneMatch counterProjection
      latestProjection emptyProjection [1, 2, 3] `shouldBe` 0

  describe "codecProjection" $ do
    it "should deserialize and apply events" $ do
      let codec = Codec show (Just . (read :: String -> Int))
          codecProj = codecProjection codec counterProjection
      latestProjection codecProj ["1", "2", "3"] `shouldBe` 6

    it "should error on deserialization failure" $ do
      let codec = Codec show (\s -> if s == "bad" then Nothing else Just (read s :: Int))
          codecProj = codecProjection codec counterProjection
      evaluate (latestProjection codecProj ["1", "bad"]) `shouldThrow` (\(DecodeError ctx _) -> ctx == "codecProjection")

  describe "lenientCodecProjection" $ do
    it "should apply deserialized events and skip failures" $ do
      let codec = Codec show (\s -> if s == "bad" then Nothing else Just (read s :: Int))
          codecProj = lenientCodecProjection codec counterProjection
      latestProjection codecProj ["1", "2", "bad", "3"] `shouldBe` 6

  describe "streamProjectionEventHandler" $ do
    it "should update state and position" $ do
      let sp = streamProjection "key" (0 :: Int) counterProjection
          event = StreamEvent "other" 5 (emptyMetadata "") (10 :: Int)
          sp' = streamProjectionEventHandler sp event
      streamProjectionState sp' `shouldBe` 10
      streamProjectionPosition sp' `shouldBe` 5
      streamProjectionKey sp' `shouldBe` "key"
