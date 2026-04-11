{-# LANGUAGE OverloadedStrings #-}

module Eventium.MetadataEnrichmentSpec (spec) where

import Control.Concurrent.STM
import Data.IORef
import Data.Maybe (isJust)
import Data.Time (UTCTime (..), fromGregorian)
import Eventium
import Eventium.Store.Memory
import Test.Hspec

spec :: Spec
spec = describe "Metadata enrichment" $ do
  describe "metadataEnrichingEventStoreWriter" $ do
    it "populates event type name from Typeable" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarTaggedEventStoreWriter tvar
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec (runEventStoreWriterUsing atomically taggedWriter)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int, 99]
      events <- reader.getEvents (allEvents uuid)
      map (\e -> e.metadata.eventType) events `shouldBe` ["Int", "Int"]

    it "populates createdAt timestamp" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarTaggedEventStoreWriter tvar
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec (runEventStoreWriterUsing atomically taggedWriter)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int]
      events <- reader.getEvents (allEvents uuid)
      all (\e -> isJust e.metadata.createdAt) events `shouldBe` True

    it "defaults occurredAt to createdAt" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarTaggedEventStoreWriter tvar
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec (runEventStoreWriterUsing atomically taggedWriter)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int]
      events <- reader.getEvents (allEvents uuid)
      -- occurredAt should default to createdAt
      map (\e -> e.metadata.occurredAt) events `shouldBe` map (\e -> e.metadata.createdAt) events

  describe "metadataEnrichingEventStoreWriterWithEnricher" $ do
    it "applies MetadataEnricher to set occurredAt" $ do
      tvar <- eventMapTVar
      let pastTime = UTCTime (fromGregorian 2025 3 15) 0
          enricher m = m {occurredAt = Just pastTime}
          taggedWriter = tvarTaggedEventStoreWriter tvar
          enrichedWriter = metadataEnrichingEventStoreWriterWithEnricher enricher testCodec (runEventStoreWriterUsing atomically taggedWriter)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int]
      events <- reader.getEvents (allEvents uuid)
      map (\e -> e.metadata.occurredAt) events `shouldBe` [Just pastTime]

    it "defaults occurredAt to createdAt when enricher does not set it" $ do
      tvar <- eventMapTVar
      let taggedWriter = tvarTaggedEventStoreWriter tvar
          enrichedWriter = metadataEnrichingEventStoreWriterWithEnricher id testCodec (runEventStoreWriterUsing atomically taggedWriter)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int]
      events <- reader.getEvents (allEvents uuid)
      -- occurredAt should default to createdAt
      map (\e -> e.metadata.occurredAt) events `shouldBe` map (\e -> e.metadata.createdAt) events

  describe "backward compatibility" $ do
    it "non-tagged writer still uses empty metadata" $ do
      tvar <- eventMapTVar
      let writer = runEventStoreWriterUsing atomically (tvarEventStoreWriter tvar)
          reader = runEventStoreReaderUsing atomically (tvarEventStoreReader tvar)
          uuid = uuidFromInteger 1
      _ <- writer.storeEvents uuid NoStream [42 :: Int]
      events <- reader.getEvents (allEvents uuid)
      map (\e -> e.metadata.eventType) events `shouldBe` [""]

  describe "publishingTaggedCodecEventStoreWriter" $ do
    it "propagates metadata to published StreamEvents" $ do
      tvar <- eventMapTVar
      publishedRef <- newIORef []
      let taggedWriter = runEventStoreWriterUsing atomically (tvarTaggedEventStoreWriter tvar)
          handler = EventHandler $ \(StreamEvent _ _ meta _) ->
            modifyIORef publishedRef (++ [meta.eventType])
          publisher = synchronousPublisher handler
          pubWriter = publishingTaggedCodecEventStoreWriter testCodec taggedWriter publisher
          enrichedWriter = metadataEnrichingEventStoreWriter testCodec pubWriter
          uuid = uuidFromInteger 1
      _ <- enrichedWriter.storeEvents uuid NoStream [42 :: Int, 99]
      published <- readIORef publishedRef
      published `shouldBe` ["Int", "Int"]

-- A simple test codec for Int
testCodec :: Codec Int Int
testCodec = Codec id Just
