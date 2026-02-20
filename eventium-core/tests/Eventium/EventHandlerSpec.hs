module Eventium.EventHandlerSpec (spec) where

import Control.Exception (evaluate)
import Data.Functor.Contravariant
import Data.IORef
import Eventium.Codec (Codec (..), DecodeError (..))
import Eventium.EventHandler
import Test.Hspec

-- Helper: create an EventHandler that appends events to an IORef
recordingHandler :: IORef [a] -> EventHandler IO a
recordingHandler ref = EventHandler $ \e -> modifyIORef ref (++ [e])

spec :: Spec
spec = do
  describe "handleEvent" $ do
    it "should process a single event" $ do
      ref <- newIORef ([] :: [Int])
      handleEvent (recordingHandler ref) 42
      readIORef ref `shouldReturn` [42]

  describe "handleEvents" $ do
    it "should process events in order" $ do
      ref <- newIORef ([] :: [Int])
      handleEvents (recordingHandler ref) [1, 2, 3]
      readIORef ref `shouldReturn` [1, 2, 3]

    it "should do nothing for empty list" $ do
      ref <- newIORef ([] :: [Int])
      handleEvents (recordingHandler ref) []
      readIORef ref `shouldReturn` []

  describe "Contravariant instance" $ do
    it "should adapt the event type via contramap" $ do
      ref <- newIORef ([] :: [Int])
      let handler = contramap length (recordingHandler ref)
      handleEvent handler "hello"
      handleEvent handler "hi"
      readIORef ref `shouldReturn` [5, 2]

  describe "Semigroup instance" $ do
    it "should run both handlers for each event" $ do
      ref1 <- newIORef ([] :: [Int])
      ref2 <- newIORef ([] :: [Int])
      let combined = recordingHandler ref1 <> recordingHandler ref2
      handleEvent combined 10
      handleEvent combined 20
      readIORef ref1 `shouldReturn` [10, 20]
      readIORef ref2 `shouldReturn` [10, 20]

  describe "Monoid instance" $ do
    it "mempty should be a no-op" $ do
      handleEvent (mempty :: EventHandler IO Int) 42
    -- No crash, no observable effect

    it "mconcat should compose multiple handlers" $ do
      ref1 <- newIORef ([] :: [Int])
      ref2 <- newIORef ([] :: [Int])
      ref3 <- newIORef ([] :: [Int])
      let combined = mconcat [recordingHandler ref1, recordingHandler ref2, recordingHandler ref3]
      handleEvent combined 99
      readIORef ref1 `shouldReturn` [99]
      readIORef ref2 `shouldReturn` [99]
      readIORef ref3 `shouldReturn` [99]

  describe "eventHandlerMapMaybe" $ do
    it "should only handle events matching the filter" $ do
      ref <- newIORef ([] :: [Int])
      let evenOnly n = if even n then Just n else Nothing
          handler = eventHandlerMapMaybe evenOnly (recordingHandler ref)
      handleEvents handler [1, 2, 3, 4, 5, 6]
      readIORef ref `shouldReturn` [2, 4, 6]

    it "should handle no events when filter matches nothing" $ do
      ref <- newIORef ([] :: [Int])
      let noneMatch _ = Nothing :: Maybe Int
          handler = eventHandlerMapMaybe noneMatch (recordingHandler ref)
      handleEvents handler [1, 2, 3]
      readIORef ref `shouldReturn` []

  describe "codecEventHandler" $ do
    it "should deserialize events before handling" $ do
      ref <- newIORef ([] :: [Int])
      let codec = Codec show (\s -> Just (read s :: Int))
          handler = codecEventHandler codec (recordingHandler ref)
      handleEvents handler ["1", "2", "3"]
      readIORef ref `shouldReturn` [1, 2, 3]

    it "should error on deserialization failure" $ do
      ref <- newIORef ([] :: [Int])
      let codec = Codec show (\s -> if s == "bad" then Nothing else Just (read s :: Int))
          handler = codecEventHandler codec (recordingHandler ref)
      handleEvents handler ["1", "bad", "3"] `shouldThrow` (\(DecodeError ctx _) -> ctx == "codecEventHandler")

  describe "lenientCodecEventHandler" $ do
    it "should silently drop events that fail to deserialize" $ do
      ref <- newIORef ([] :: [Int])
      let codec = Codec show (\s -> if s == "bad" then Nothing else Just (read s :: Int))
          handler = lenientCodecEventHandler codec (recordingHandler ref)
      handleEvents handler ["1", "bad", "3"]
      readIORef ref `shouldReturn` [1, 3]
