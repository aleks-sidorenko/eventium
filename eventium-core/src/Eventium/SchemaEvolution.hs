-- | Event schema evolution via upcast-on-read against an immutable log.
--
-- Stored events are never mutated. Instead, when an event type's shape changes
-- across a release, older stored events are normalized to the current shape
-- /on read/ by running a chain of pure, single-hop upcasters. This preserves the
-- append-only event log (the source of truth), makes @pg_dump@/restore
-- version-independent, and supports version-skipping (restoring an
-- N-releases-old backup) simply by running more hops.
--
-- This module is the entry point; it re-exports:
--
--   * "Eventium.SchemaEvolution.Types" — the representation-agnostic __core__:
--     'SchemaRegistry', 'registerUpcasters', 'currentVersion', and 'upcast',
--     parametric in a document representation @d@ ('Upcaster' @d = d -> d@).
--     The chaining, versioning, and version-skipping logic live here.
--   * "Eventium.SchemaEvolution.Json" — the JSON __instance__ over Aeson @Value@:
--     the versioned envelope, the field combinators ('atKey', 'addFieldIfAbsent',
--     …), and 'upcastingValueCodec'.
--
-- Import a submodule directly to limit scope; import this module for both.
--
-- == Registry
--
-- A 'SchemaRegistry' maps an 'EventTypeName' to an ordered list of single-hop
-- upcasters @[v1->v2, v2->v3, ...]@. The current version of a type is
-- @1 + number of hops@, so the two can never drift. A type with no registered
-- upcasters is at version 1 and 'upcast's to itself.
--
-- == Envelope
--
-- Serialized JSON payloads are wrapped in a versioned envelope
-- @{ "schemaVersion": N, "payload": <event-json> }@. Events written before the
-- envelope existed lack the key and are read as @schemaVersion = 1@; existing
-- rows are never rewritten.
--
-- == Representation scope
--
-- The core is parametric in @d@ so the machinery is stable across serializations,
-- but only JSON is shipped: every persistent store serializes to JSON
-- (@eventium-postgresql@ / @eventium-sqlite@ store @JSONString@); the in-memory
-- store keeps native values and never serializes, so schema evolution does not
-- apply there. Upcast-on-read is only meaningful against a __self-describing__
-- representation (you must rewrite fields of old data whose Haskell type may be
-- gone). A future self-describing format (e.g. CBOR @Term@) instantiates @d@ with
-- its own envelope + combinators, core untouched. Protobuf is deliberately out of
-- this path: non-self-describing wire + native field evolution (no proto3
-- required fields), so proto handles add\/remove itself and would use the
-- registry only for the /semantic/ hops it can't express.
module Eventium.SchemaEvolution
  ( module Eventium.SchemaEvolution.Types,
    module Eventium.SchemaEvolution.Json,
  )
where

import Eventium.SchemaEvolution.Json
import Eventium.SchemaEvolution.Types
