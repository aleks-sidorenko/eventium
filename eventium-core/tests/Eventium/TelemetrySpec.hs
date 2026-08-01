{-# LANGUAGE OverloadedStrings #-}

module Eventium.TelemetrySpec (spec) where

import Data.IORef
import Eventium.Store.Types (emptyMetadata)
import Eventium.Telemetry
import Test.Hspec

spec :: Spec
spec = describe "Telemetry" $ do
  it "silentTelemetry emits nothing (runs cleanly, returns unit)" $ do
    let t = silentTelemetry :: Telemetry IO
    t.emit (EventsPersisted (StreamKeyText "s") [emptyMetadata "Foo"] []) `shouldReturn` ()

  it "a capturing sink records the signal it is given" $ do
    ref <- newIORef []
    let t = Telemetry (\sig -> modifyIORef' ref (sig :)) :: Telemetry IO
    let sig = EventsPersisted (StreamKeyText "s") [emptyMetadata "Foo"] []
    t.emit sig
    readIORef ref `shouldReturn` [sig]
