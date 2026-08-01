{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides list-based command routing for multi-aggregate systems.
--
-- Instead of manually trying each command handler in a nested cascade,
-- collect handlers into a list and let 'commandHandlerDispatcher' route
-- commands automatically.
module Eventium.CommandDispatcher
  ( AggregateHandler,
    mkAggregateHandler,
    mkAggregateHandlerWith,
    commandHandlerDispatcher,
    commandHandlerDispatcherWithTag,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Eventium.Codec (Codec)
import Eventium.CommandHandler (CommandHandler, CommandHandlerError (..), applyCommandHandler)
import Eventium.ProcessManager (CommandDispatchResult (..), CommandDispatcher (..), RejectionReason (..))
import Eventium.Store.Class (EventStoreWriter, VersionedEventStoreReader, metadataEnrichingEventStoreWriterWithTag)
import Eventium.Store.Types (EventTypeName, EventVersion, TaggedEvent, eventTypeNameOf)
import Eventium.UUID (UUID)

-- | An embedded command handler paired with an error formatter.
--
-- Erases the aggregate state type and error type so that multiple
-- aggregate handlers can be collected in a homogeneous list for routing.
data AggregateHandler event command
  = forall state err.
    AggregateHandler
      (CommandHandler state event command err)
      (err -> RejectionReason)

-- | Construct an 'AggregateHandler' using 'Show' to format errors.
mkAggregateHandler ::
  (Show err) =>
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandler h = AggregateHandler h (RejectionReason . T.pack . show)

-- | Construct an 'AggregateHandler' with an explicit error formatter.
mkAggregateHandlerWith ::
  (err -> RejectionReason) ->
  CommandHandler state event command err ->
  AggregateHandler event command
mkAggregateHandlerWith fmt h = AggregateHandler h fmt

-- | Build a 'CommandDispatcher' from a list of 'AggregateHandler's.
--
-- Accepts a tagged writer and codec; the 'MetadataEnricher' supplied at
-- dispatch time is applied to create a per-dispatch enriched writer.
--
-- Tries each handler in order:
--
--   * @Right (e:es)@ — command matched and produced events → 'CommandSucceeded'
--   * @Left (CommandRejected err)@ — command matched but was rejected → 'CommandFailed'
--   * @Left (ConcurrencyConflict _)@ — optimistic locking failure → 'CommandFailed'
--   * @Right []@ — command did not match this handler → try next
--
-- If no handler matches (all return @Right []@), reports 'CommandSucceeded' (no-op).
--
-- Tags each emitted event's 'EventMetadata.eventType' via 'Typeable'. If the
-- event type is an application-wide sum (e.g. @AccountingEvent@) whose
-- Typeable name isn't the useful discriminator, use
-- 'commandHandlerDispatcherWithTag' instead.
commandHandlerDispatcher ::
  (MonadIO m, Typeable event) =>
  Codec event encoded ->
  EventStoreWriter UUID EventVersion m (TaggedEvent encoded) ->
  VersionedEventStoreReader m event ->
  [AggregateHandler event command] ->
  CommandDispatcher m command
commandHandlerDispatcher = commandHandlerDispatcherWithTag eventTypeNameOf

-- | Like 'commandHandlerDispatcher' but the caller supplies the
-- 'EventTypeName' per event (instead of deriving it from 'Typeable'). Use
-- when the event is a wrapper sum whose Typeable name isn't the useful
-- discriminator — e.g. an application-wide event sum type such as
-- @AccountingEvent@, where every value shares the same Typeable name
-- regardless of which case it wraps. This matters for saga/process-manager
-- emitted events, which are routed through this dispatcher.
commandHandlerDispatcherWithTag ::
  (MonadIO m) =>
  (event -> EventTypeName) ->
  Codec event encoded ->
  EventStoreWriter UUID EventVersion m (TaggedEvent encoded) ->
  VersionedEventStoreReader m event ->
  [AggregateHandler event command] ->
  CommandDispatcher m command
commandHandlerDispatcherWithTag tagOf codec taggedWriter reader handlers =
  CommandDispatcher $ \uuid cmd enricher ->
    let writer = metadataEnrichingEventStoreWriterWithTag tagOf enricher codec taggedWriter
     in go handlers writer uuid cmd
  where
    go [] _ _ _ = pure CommandSucceeded
    go (AggregateHandler handler formatErr : rest) writer uuid cmd = do
      result <- applyCommandHandler writer reader handler uuid cmd
      case result of
        Right (_ : _) -> pure CommandSucceeded
        Left (CommandRejected err) -> pure (CommandFailed (formatErr err))
        Left (ConcurrencyConflict _) -> pure (CommandFailed "Concurrency conflict")
        Right [] -> go rest writer uuid cmd
