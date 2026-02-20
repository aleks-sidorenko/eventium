{-# LANGUAGE OverloadedStrings #-}

module Eventium.MetadataEnrichmentSpec (spec) where

import Control.Concurrent.STM
import Data.IORef
import Data.Maybe (isJust)
import Eventium
import Eventium.Store.Memory
import Test.Hspec

spec :: Spec
spec = describe "Metadata enrichment" $ do
  describe "metadataEnrichingEventStoreWriter" $ do
    it "populates event type name from Typeable" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarEventStoreWriterTagged tvar
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec (runEventStoreWriterUsing atomically taggedWriter)
          uuid = uuidFromInteger 1
      _ <- storeEvents enrichedWriter uuid NoStream [42 :: Int, 99]
      events <- getEvents (runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)) (allEvents uuid)
      map (eventMetadataEventType . streamEventMetadata) events `shouldBe` ["Int", "Int"]

    it "populates createdAt timestamp" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarEventStoreWriterTagged tvar
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec (runEventStoreWriterUsing atomically taggedWriter)
          uuid = uuidFromInteger 1
      _ <- storeEvents enrichedWriter uuid NoStream [42 :: Int]
      events <- getEvents (runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)) (allEvents uuid)
      all (isJust . eventMetadataCreatedAt . streamEventMetadata) events `shouldBe` True

  describe "backward compatibility" $ do
    it "non-tagged writer still uses empty metadata" $ do
      tvar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- storeEvents writer uuid NoStream [42 :: Int]
      events <- getEvents reader (allEvents uuid)
      map (eventMetadataEventType . streamEventMetadata) events `shouldBe` [""]

  describe "publishingEventStoreWriterTagged" $ do
    it "propagates metadata to published StreamEvents" $ do
      tvar <- eventMapTVar
      publishedRef <- newIORef []
      let taggedWriter = runEventStoreWriterUsing atomically (tvarEventStoreWriterTagged tvar)
          handler = EventHandler $ \(StreamEvent _ _ meta _) ->
            modifyIORef publishedRef (++ [eventMetadataEventType meta])
          publisher = synchronousPublisher handler
          pubWriter = publishingEventStoreWriterTagged taggedWriter publisher
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec pubWriter
          uuid = uuidFromInteger 1
      _ <- storeEvents enrichedWriter uuid NoStream [42 :: Int, 99]
      published <- readIORef publishedRef
      published `shouldBe` ["Int", "Int"]

-- A simple test codec for Int
testCodec :: Codec Int Int
testCodec = Codec id Just
