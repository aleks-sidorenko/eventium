-- | Representation-agnostic core of event schema evolution (see
-- "Eventium.SchemaEvolution" for the overview and the JSON instance).
--
-- 'SchemaRegistry', 'registerUpcasters', 'currentVersion', and 'upcast' are
-- parametric in a document representation @d@ ('Upcaster' @d = d -> d@) and never
-- inspect it. The chaining, versioning, and version-skipping logic all live here
-- and are shared by every representation; only the envelope and field
-- combinators are representation-specific (e.g. "Eventium.SchemaEvolution.Json").
module Eventium.SchemaEvolution.Types
  ( -- * Versions and upcasters
    EventTypeName,
    SchemaVersion,
    Upcaster,

    -- * Registry
    SchemaRegistry,
    emptyRegistry,
    registerUpcasters,
    currentVersion,

    -- * Applying the chain
    upcast,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Eventium.Store.Types (EventTypeName)

-- | The schema version of an event type. Version numbering starts at 1.
type SchemaVersion = Int

-- | A pure, single-hop transform from one schema version to the next, over a
-- serialization's document representation @d@. The registry and chaining are
-- parametric in @d@ and never inspect it; only the envelope and field
-- combinators (per representation, e.g. "Eventium.SchemaEvolution.Json") are
-- representation-specific.
type Upcaster d = d -> d

-- | A registry of upcaster chains, keyed by event-type name, over a document
-- representation @d@. The list for each type is ordered @[v1->v2, v2->v3, ...]@.
-- The registry is parametric in @d@ and never inspects it.
newtype SchemaRegistry d = SchemaRegistry (Map EventTypeName [Upcaster d])

-- | A registry with no upcasters: every event type is at version 1.
emptyRegistry :: SchemaRegistry d
emptyRegistry = SchemaRegistry Map.empty

-- | Register the ordered upcaster chain for an event type, replacing any
-- existing entry. The list must be ordered @[v1->v2, v2->v3, ...]@; its length
-- determines the type's current version.
registerUpcasters :: EventTypeName -> [Upcaster d] -> SchemaRegistry d -> SchemaRegistry d
registerUpcasters eventType hops (SchemaRegistry m) =
  SchemaRegistry (Map.insert eventType hops m)

-- | The current schema version of an event type: @1 + number of hops@. An
-- unregistered type is at version 1.
currentVersion :: SchemaRegistry d -> EventTypeName -> SchemaVersion
currentVersion reg eventType = 1 + length (hopsOf reg eventType)

-- | Apply the upcaster chain that brings an event of @eventType@ stored at
-- @version@ up to the current shape: run the hops not yet baked into the stored
-- version, in order. Representation-agnostic — it only composes the registered
-- @d -> d@ hops.
upcast :: SchemaRegistry d -> EventTypeName -> SchemaVersion -> d -> d
upcast reg eventType version x =
  foldl (\acc hop -> hop acc) x (hopsFrom reg eventType version)

-- | All registered hops for an event type (@[]@ if unregistered).
hopsOf :: SchemaRegistry d -> EventTypeName -> [Upcaster d]
hopsOf (SchemaRegistry m) eventType = Map.findWithDefault [] eventType m

-- | The hops needed to bring an event stored at @version@ up to current: drop
-- the @version - 1@ hops already baked into the stored shape.
hopsFrom :: SchemaRegistry d -> EventTypeName -> SchemaVersion -> [Upcaster d]
hopsFrom reg eventType version = drop (max 0 (version - 1)) (hopsOf reg eventType)
