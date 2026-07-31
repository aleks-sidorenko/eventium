{-# LANGUAGE OverloadedStrings #-}

-- | The JSON instance of event schema evolution (see "Eventium.SchemaEvolution"
-- for the representation-agnostic core): the versioned envelope, field-level
-- upcaster combinators, and the upcasting 'Codec' — all over Aeson 'Value'.
--
-- == Envelope
--
-- Serialized payloads are wrapped in a versioned envelope:
--
-- > { "schemaVersion": N, "payload": <event-json> }
--
-- Events written before this envelope existed lack the top-level
-- @schemaVersion@ key and are defined to be @schemaVersion = 1@ by convention;
-- their whole stored value is the payload. Existing rows are never rewritten.
--
-- == Codec
--
-- 'upcastingValueCodec' is a drop-in 'Codec' between a domain event type and its
-- stored 'Value'. It is parameterized by an @eventTypeOf@ function that reads the
-- type name from a payload (the caller's tagging convention), which keeps it
-- agnostic to how any particular app tags its events. Because it is a plain
-- 'Codec', it serves both synchronous (in-transaction) and asynchronous
-- (polling) consumers, and every event type and backend, with no reader-level
-- plumbing.
module Eventium.SchemaEvolution.Json
  ( -- * JSON upcasters
    JsonUpcaster,

    -- * Field combinators
    atKey,
    addFieldIfAbsent,
    renameField,
    removeField,

    -- * Envelope
    wrapEnvelope,
    unwrapEnvelope,

    -- * Codec
    upcastingValueCodec,
  )
where

import Data.Aeson
  ( FromJSON,
    Result (..),
    ToJSON,
    Value (..),
    fromJSON,
    object,
    toJSON,
    (.=),
  )
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Eventium.Codec (Codec (..))
import Eventium.SchemaEvolution.Types
  ( EventTypeName,
    SchemaRegistry,
    SchemaVersion,
    Upcaster,
    currentVersion,
    upcast,
  )

-- | An 'Upcaster' over the JSON document model (Aeson 'Value') — the one
-- representation eventium ships.
type JsonUpcaster = Upcaster Value

-- -----------------------------------------------------------------------------
-- Field combinators
-- -----------------------------------------------------------------------------

-- These build the common field-level transforms without hand-rolled 'KeyMap'
-- manipulation. Each is a no-op on non-object values, so they compose freely
-- (e.g. focus into a nested object with 'atKey', then edit a field there).

-- | Apply an upcaster to the value at @key@ within an object, leaving the rest
-- untouched. A no-op if @key@ is absent, or if the value is not an object.
-- Use this to focus a field-level transform into a nested object (e.g. an
-- Aeson tagged constructor's @contents@).
atKey :: Key -> JsonUpcaster -> JsonUpcaster
atKey key f (Object o) = case KeyMap.lookup key o of
  Just v -> Object (KeyMap.insert key (f v) o)
  Nothing -> Object o
atKey _ _ v = v

-- | Insert @key = value@ only if @key@ is not already present. Idempotent and
-- never clobbers an existing value — the safe default for "a new field was
-- added with default @value@".
addFieldIfAbsent :: Key -> Value -> JsonUpcaster
addFieldIfAbsent key value (Object o) = Object (KeyMap.insertWith (\_new old -> old) key value o)
addFieldIfAbsent _ _ v = v

-- | Move the value at @from@ to @to@ (overwriting @to@ if present), dropping
-- @from@. A no-op if @from@ is absent.
renameField :: Key -> Key -> JsonUpcaster
renameField from to (Object o) = case KeyMap.lookup from o of
  Just v -> Object (KeyMap.insert to v (KeyMap.delete from o))
  Nothing -> Object o
renameField _ _ v = v

-- | Remove @key@ from an object. A no-op if @key@ is absent.
removeField :: Key -> JsonUpcaster
removeField key (Object o) = Object (KeyMap.delete key o)
removeField _ v = v

-- -----------------------------------------------------------------------------
-- Envelope
-- -----------------------------------------------------------------------------

envelopeVersionKey :: Key
envelopeVersionKey = "schemaVersion"

envelopePayloadKey :: Key
envelopePayloadKey = "payload"

-- | Wrap a payload in a versioned envelope.
wrapEnvelope :: SchemaVersion -> Value -> Value
wrapEnvelope version payload =
  object [envelopeVersionKey .= version, envelopePayloadKey .= payload]

-- | Peel an envelope into @(version, payload)@. A value that is not a
-- well-formed envelope — anything without both a numeric top-level
-- @schemaVersion@ and a @payload@ sibling — is legacy pre-envelope data and is
-- read as @(1, wholeValue)@.
unwrapEnvelope :: Value -> (SchemaVersion, Value)
unwrapEnvelope (Object o)
  | Just version <- KeyMap.lookup envelopeVersionKey o >>= valueToInt,
    Just payload <- KeyMap.lookup envelopePayloadKey o =
      (version, payload)
unwrapEnvelope v = (1, v)

valueToInt :: Value -> Maybe SchemaVersion
valueToInt v = case fromJSON v of
  Success n -> Just n
  Error _ -> Nothing

-- -----------------------------------------------------------------------------
-- Codec
-- -----------------------------------------------------------------------------

-- | A drop-in 'Codec' between a domain event type and its stored 'Value',
-- applying schema evolution transparently.
--
-- On 'encode', the current-version envelope is written. On 'decode', the
-- envelope is peeled (or defaulted to version 1 for legacy data), the event
-- type is resolved via @eventTypeOf@, the remaining upcaster hops are run via
-- 'upcast', and the result is decoded into the current type. Decoding yields
-- 'Nothing' if the upcasted value still does not match the current type.
upcastingValueCodec ::
  (ToJSON a, FromJSON a) =>
  -- | Read the event-type name from a payload (the app's tagging convention).
  (Value -> Maybe EventTypeName) ->
  SchemaRegistry Value ->
  Codec a Value
upcastingValueCodec eventTypeOf reg =
  Codec
    { encode = \a ->
        let payload = toJSON a
            version = maybe 1 (currentVersion reg) (eventTypeOf payload)
         in wrapEnvelope version payload,
      decode = \stored ->
        let (version, payload) = unwrapEnvelope stored
            upcasted = case eventTypeOf payload of
              Just eventType -> upcast reg eventType version payload
              Nothing -> payload
         in resultToMaybe (fromJSON upcasted)
    }
  where
    resultToMaybe (Success a) = Just a
    resultToMaybe (Error _) = Nothing
