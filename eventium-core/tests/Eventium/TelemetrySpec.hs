{-# LANGUAGE OverloadedStrings #-}

module Eventium.TelemetrySpec (spec) where

import Data.IORef
import Eventium.Store.Types (emptyMetadata)
import Eventium.Telemetry
import Test.Hspec

spec :: Spec
spec = describe "Telemetry" $ do
  it "silentTelemetry emits nothing" $ do
    ref <- newIORef (0 :: Int)
    let t = silentTelemetry :: Telemetry IO
    t.emit (EventsPersisted (StreamKeyText "s") [emptyMetadata "Foo"] [])
    readIORef ref `shouldReturn` 0

  it "a capturing sink records the signal it is given" $ do
    ref <- newIORef []
    let t = Telemetry (\sig -> modifyIORef' ref (sig :)) :: Telemetry IO
    let sig = EventsPersisted (StreamKeyText "s") [emptyMetadata "Foo"] []
    t.emit sig
    readIORef ref `shouldReturn` [sig]
