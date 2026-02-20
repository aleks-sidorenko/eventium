{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pure data types for the event store. These types have no dependency on
-- any monad or effect system. They are extracted from "Eventium.Store.Class"
-- so that pure modules (like "Eventium.Projection") can reference them without
-- pulling in effectful store interfaces.
module Eventium.Store.Types
  ( -- * Stream events
    StreamEvent (..),
    VersionedStreamEvent,
    GlobalStreamEvent,

    -- * Event metadata
    EventMetadata (..),
    emptyMetadata,
    TaggedEvent (..),

    -- * Expected position
    ExpectedPosition (..),
    EventWriteError (..),

    -- * Utility types
    EventVersion (..),
    SequenceNumber (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Eventium.UUID
import Web.HttpApiData
import Web.PathPieces

-- | Metadata carried alongside every stored event.
data EventMetadata = EventMetadata
  { eventMetadataEventType :: !Text,
    eventMetadataCorrelationId :: !(Maybe UUID),
    eventMetadataCausationId :: !(Maybe UUID),
    eventMetadataCreatedAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq)

-- | Construct 'EventMetadata' with only an event type name.
emptyMetadata :: Text -> EventMetadata
emptyMetadata et = EventMetadata et Nothing Nothing Nothing

-- | An event paired with pre-computed metadata. Used to thread metadata
-- through the 'EventStoreWriter' interface without changing its type signature.
-- Store backends that accept @TaggedEvent@ will use the attached metadata
-- instead of generating empty metadata.
data TaggedEvent event = TaggedEvent
  { taggedEventMetadata :: !EventMetadata,
    taggedEventPayload :: !event
  }
  deriving (Show, Eq, Functor)

-- | An event along with the @key@ for the event stream it is from, its
-- @position@ in that event stream, and 'EventMetadata'.
data StreamEvent key position event
  = StreamEvent
  { streamEventKey :: !key,
    streamEventPosition :: !position,
    streamEventMetadata :: !EventMetadata,
    streamEventEvent :: !event
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type VersionedStreamEvent event = StreamEvent UUID EventVersion event

type GlobalStreamEvent event = StreamEvent () SequenceNumber (VersionedStreamEvent event)

-- | ExpectedPosition is used to assert the event stream is at a certain
-- position. This is used when multiple writers are concurrently writing to the
-- event store. If the expected position is incorrect, then storing fails.
data ExpectedPosition position
  = -- | Used when the writer doesn't care what position the stream is at.
    AnyPosition
  | -- | The stream shouldn't exist yet.
    NoStream
  | -- | The stream should already exist.
    StreamExists
  | -- | Used to assert the stream is at a particular position.
    ExactPosition position
  deriving (Show, Eq)

newtype EventWriteError position
  = EventStreamNotAtExpectedVersion position
  deriving (Show, Eq)

-- | Event versions are a strictly increasing series of integers for each
-- projection. They allow us to order the events when they are replayed, and
-- they also help as a concurrency check in a multi-threaded environment so
-- services modifying the projection can be sure the projection didn't change
-- during their execution.
newtype EventVersion = EventVersion {unEventVersion :: Int}
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON)

-- | The sequence number gives us a global ordering of events in a particular
-- event store. Using sequence numbers is not strictly necessary for an event
-- sourcing and CQRS system, but it makes it way easier to replay events
-- consistently without having to use distributed transactions in an event bus.
-- In SQL-based event stores, they are also very cheap to create.
newtype SequenceNumber = SequenceNumber {unSequenceNumber :: Int}
  deriving
    ( Show,
      Read,
      Ord,
      Eq,
      Enum,
      Num,
      FromJSON,
      ToJSON,
      PathPiece,
      ToHttpApiData,
      FromHttpApiData
    )
