-- | Write-path telemetry: a decorator that emits a 'Signal' on each versioned
-- write outcome. Specialized to the versioned (aggregate) path so the stream
-- key ('UUID') and positions ('EventVersion') are concrete.
module Eventium.Store.Telemetry
  ( telemetryEventStoreWriter,
  )
where

import Eventium.Store.Class (EventStoreWriter (..), VersionedEventStoreWriter)
import Eventium.Store.Types
  ( EventWriteError (..),
    TaggedEvent (..),
  )
import Eventium.Telemetry

-- | Wrap a versioned 'TaggedEvent' writer so it emits 'EventsPersisted' on a
-- successful write and 'WriteConflict' on an optimistic-concurrency failure.
-- An empty batch emits nothing. A store-level exception is not reported (the
-- decorator does not bracket). The stream 'UUID' is carried through as-is.
telemetryEventStoreWriter ::
  (Monad m) =>
  Telemetry m ->
  VersionedEventStoreWriter m (TaggedEvent encoded) ->
  VersionedEventStoreWriter m (TaggedEvent encoded)
telemetryEventStoreWriter telemetry (EventStoreWriter write) =
  EventStoreWriter $ \key expectedPos events -> do
    result <- write key expectedPos events
    case events of
      [] -> pure ()
      _ -> case result of
        Right wr -> telemetry.emit (EventsPersisted key (map (.metadata) events) wr)
        Left (EventStreamNotAtExpectedVersion actualPos) ->
          telemetry.emit (WriteConflict key (ConflictInfo expectedPos actualPos))
    pure result
