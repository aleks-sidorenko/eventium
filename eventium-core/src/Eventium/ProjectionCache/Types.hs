module Eventium.ProjectionCache.Types
  ( ProjectionCache (..),
    VersionedProjectionCache,
    GlobalProjectionCache,
  )
where

import Eventium.Store.Types
import Eventium.UUID

-- | A 'ProjectionCache' caches snapshots of 'Projection's in event streams.
-- This is useful if your event streams are very large. This cache operates on
-- some 'Monad' @m@ and stores the 'Projection' state of type @encoded@.
--
-- At its core, this is essentially just a key-value store with knowledge of
-- the stream 'UUID' and 'EventVersion'. It is recommended to use the other
-- helper functions in this module to interpret the stored values using a
-- 'Projection'.
--
-- The @key@ and @position@ type parameters are polymorphic so we can abstract
-- over a cache for individual event streams, and a cache for globally ordered
-- streams.
data ProjectionCache key position encoded m
  = ProjectionCache
  { -- | Stores the state for a projection at a given @key@ and @position@.
    -- This is pretty unsafe, because there is no guarantee what is stored is
    -- actually derived from the events in the stream. Consider using
    -- 'updateVersionedProjectionCache'.
    storeSnapshot :: key -> position -> encoded -> m (),
    -- | Loads the latest projection state from the cache.
    loadSnapshot :: key -> m (Maybe (position, encoded))
  }

-- | Type synonym for a 'ProjectionCache' used on individual event streams.
type VersionedProjectionCache encoded m = ProjectionCache UUID EventVersion encoded m

-- | Type synonym for a 'ProjectionCache' that is used in conjunction with a
-- 'GlobalStreamEventStore'.
type GlobalProjectionCache encoded m = ProjectionCache () SequenceNumber encoded m
