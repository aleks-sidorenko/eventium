-- | A generic, framework-free, structured telemetry sink. The host app
-- supplies one interpreter ('Telemetry'); eventium emits typed 'Signal's
-- through it. No logging-framework dependency — a contravariant-style sink over
-- a domain signal type.
module Eventium.Telemetry
  ( Telemetry (..),
    Signal (..),
    ConflictInfo (..),
    StreamKeyText (..),
    silentTelemetry,
  )
where

import Data.Text (Text)
import Eventium.Store.Types
  ( EventMetadata,
    EventVersion,
    EventWriteResult,
    ExpectedPosition,
  )

-- | A structured telemetry sink. @emit@ runs in the caller's monad @m@.
-- Interpreters MUST NOT throw: a write-path emit may run inside the write
-- transaction, so a throwing interpreter could roll back a committed write.
newtype Telemetry m = Telemetry {emit :: Signal -> m ()}

-- | A rendered event-stream key (streams may key on any type; the write
-- decorator renders 'UUID' streams to text at the emit site).
newtype StreamKeyText = StreamKeyText Text
  deriving (Show, Eq)

-- | Everything eventium can report, across all subsystems. One growing closed
-- sum type. This slice introduces only the write-path constructors.
data Signal
  = -- | Events durably written on the versioned (aggregate) write path:
    -- stream key, per-event metadata (each carries @eventType@, @correlationId@,
    -- @custom@), and assigned per-stream versions + global positions.
    EventsPersisted !StreamKeyText ![EventMetadata] !EventWriteResult
  | -- | An expected-position (optimistic concurrency) check failed; nothing
    -- was written.
    WriteConflict !StreamKeyText !ConflictInfo
  deriving (Show, Eq)

-- | Optimistic-concurrency conflict detail. @expected@ is the caller's asserted
-- position; @actual@ is the stream's real end version.
data ConflictInfo = ConflictInfo
  { expected :: !(ExpectedPosition EventVersion),
    actual :: !EventVersion
  }
  deriving (Show, Eq)

-- | No-op sink — the default everywhere; guarantees silent, zero-cost behaviour
-- unless the app opts in.
silentTelemetry :: (Applicative m) => Telemetry m
silentTelemetry = Telemetry (const (pure ()))
