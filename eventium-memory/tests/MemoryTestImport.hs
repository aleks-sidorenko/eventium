module MemoryTestImport
  ( EmbeddedState (..),
    StreamEmbeddedState,
    GlobalStreamEmbeddedState,
    emptyEmbeddedState,
    setEventMap,
    setProjectionMap,
  )
where

import Eventium.ProjectionCache.Memory
import Eventium.Store.Memory
import Eventium.UUID

data EmbeddedState state event key position
  = EmbeddedState
  { dummyArgument :: Int,
    eventMap :: EventMap event,
    projectionMap :: ProjectionMap key position state
  }

type StreamEmbeddedState state event = EmbeddedState state event UUID EventVersion

type GlobalStreamEmbeddedState state event key = EmbeddedState state event key SequenceNumber

emptyEmbeddedState :: EmbeddedState state event key position
emptyEmbeddedState = EmbeddedState 100 emptyEventMap emptyProjectionMap

setEventMap :: EmbeddedState state event key position -> EventMap event -> EmbeddedState state event key position
setEventMap state' em = state' {eventMap = em}

setProjectionMap ::
  EmbeddedState state event key position ->
  ProjectionMap key position state ->
  EmbeddedState state event key position
setProjectionMap state' pm = state' {projectionMap = pm}
