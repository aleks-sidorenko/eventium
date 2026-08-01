{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.TelemetrySpec (spec) where

import Data.IORef
import Data.Text (Text)
import Eventium.Store.Class (EventStoreWriter (..))
import Eventium.Store.Telemetry (telemetryEventStoreWriter)
import Eventium.Store.Types
  ( EventVersion (..),
    EventWriteError (..),
    ExpectedPosition (..),
    SequenceNumber (..),
    TaggedEvent (..),
    emptyMetadata,
  )
import Eventium.Telemetry
import qualified Eventium.UUID as UUID
import Test.Hspec

capturing :: IO (IORef [Signal], Telemetry IO)
capturing = do
  ref <- newIORef []
  pure (ref, Telemetry (\s -> modifyIORef' ref (++ [s])))

ev :: TaggedEvent Text
ev = TaggedEvent (emptyMetadata "Foo") "payload"

key :: UUID.UUID
key = UUID.nil

spec :: Spec
spec = describe "telemetryEventStoreWriter" $ do
  it "emits EventsPersisted on a successful write" $ do
    (ref, t) <- capturing
    let wr = [(EventVersion 1, SequenceNumber 10)]
    let inner = EventStoreWriter (\_ _ _ -> pure (Right wr))
    _ <- (telemetryEventStoreWriter t inner).storeEvents key AnyPosition [ev]
    signals <- readIORef ref
    signals `shouldBe` [EventsPersisted (StreamKeyText (UUID.uuidToText key)) [emptyMetadata "Foo"] wr]

  it "emits WriteConflict on an expected-position failure" $ do
    (ref, t) <- capturing
    let inner = EventStoreWriter (\_ _ _ -> pure (Left (EventStreamNotAtExpectedVersion (EventVersion 7))))
    _ <- (telemetryEventStoreWriter t inner).storeEvents key (ExactPosition (EventVersion 3)) [ev]
    signals <- readIORef ref
    signals `shouldBe` [WriteConflict (StreamKeyText (UUID.uuidToText key)) (ConflictInfo (ExactPosition (EventVersion 3)) (EventVersion 7))]

  it "emits nothing for an empty batch (even on a Left)" $ do
    (ref, t) <- capturing
    let inner = EventStoreWriter (\_ _ _ -> pure (Left (EventStreamNotAtExpectedVersion (EventVersion 7))))
    _ <- (telemetryEventStoreWriter t inner).storeEvents key (ExactPosition (EventVersion 3)) ([] :: [TaggedEvent Text])
    readIORef ref `shouldReturn` []

  it "silentTelemetry emits nothing" $ do
    (ref, _) <- capturing
    let wr = [(EventVersion 1, SequenceNumber 10)]
    let inner = EventStoreWriter (\_ _ _ -> pure (Right wr))
    _ <- (telemetryEventStoreWriter silentTelemetry inner).storeEvents key AnyPosition [ev]
    readIORef ref `shouldReturn` []
