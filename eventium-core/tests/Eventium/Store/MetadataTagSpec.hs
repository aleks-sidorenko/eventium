{-# LANGUAGE OverloadedStrings #-}

module Eventium.Store.MetadataTagSpec (spec) where

import Data.IORef
import Data.Text (Text)
import Eventium.Codec (idCodec)
import Eventium.Store.Class
  ( EventStoreWriter (..),
    metadataEnrichingEventStoreWriterWithEnricher,
    metadataEnrichingEventStoreWriterWithTag,
  )
import Eventium.Store.Types
  ( EventMetadata (..),
    ExpectedPosition (..),
    TaggedEvent (..),
  )
import qualified Eventium.UUID as UUID
import Test.Hspec

capturing :: IO (IORef [TaggedEvent Text], EventStoreWriter UUID.UUID Int IO (TaggedEvent Text))
capturing = do
  ref <- newIORef []
  let writer = EventStoreWriter $ \_ _ events -> do
        modifyIORef' ref (++ events)
        pure (Right [])
  pure (ref, writer)

key :: UUID.UUID
key = UUID.nil

spec :: Spec
spec = describe "metadata event-type tagging" $ do
  it "metadataEnrichingEventStoreWriterWithTag carries the caller-supplied tag" $ do
    (ref, inner) <- capturing
    let wr = metadataEnrichingEventStoreWriterWithTag (const "SpecificTag") id idCodec inner
    _ <- wr.storeEvents key AnyPosition ["payload" :: Text]
    [tagged] <- readIORef ref
    tagged.metadata.eventType `shouldBe` ("SpecificTag" :: Text)

  it "metadataEnrichingEventStoreWriterWithEnricher still uses the Typeable name (unchanged)" $ do
    (ref, inner) <- capturing
    let wr = metadataEnrichingEventStoreWriterWithEnricher id idCodec inner
    _ <- wr.storeEvents key AnyPosition ["payload" :: Text]
    [tagged] <- readIORef ref
    tagged.metadata.eventType `shouldBe` ("Text" :: Text)
