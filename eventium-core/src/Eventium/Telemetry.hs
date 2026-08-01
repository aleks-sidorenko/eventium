-- | A generic, framework-free, structured telemetry sink. The host app
-- supplies one interpreter ('Telemetry'); eventium emits typed 'Signal's
-- through it. No logging-framework dependency — a contravariant-style sink over
-- a domain signal type.
module Eventium.Telemetry
  ( Telemetry (..),
    Signal (..),
    ConflictInfo (..),
    silentTelemetry,
  )
where

import Eventium.Store.Types
  ( EventMetadata,
    EventVersion,
    EventWriteResult,
    ExpectedPosition,
  )
import Eventium.UUID (UUID)

-- | A structured telemetry sink. @emit@ runs in the caller's monad @m@.
-- Interpreters MUST NOT throw: a write-path emit may run inside the write
-- transaction, so a throwing interpreter could roll back a committed write.
newtype Telemetry m = Telemetry {emit :: Signal -> m ()}

-- | Everything eventium can report, across all subsystems. One growing closed
-- sum type. This slice introduces only the write-path constructors.
--
-- Write-path constructors carry the versioned-stream key as a 'UUID' (the
-- aggregate id), eventium's own stream identifier — interpreters render it as
-- they see fit.
data Signal
  = -- | Events durably written on the versioned (aggregate) write path:
    -- stream 'UUID', per-event metadata (each carries @eventType@,
    -- @correlationId@, @custom@), and assigned per-stream versions + global
    -- positions.
    EventsPersisted !UUID ![EventMetadata] !EventWriteResult
  | -- | An expected-position (optimistic concurrency) check failed; nothing
    -- was written.
    WriteConflict !UUID !ConflictInfo
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
