module Eventium.CheckpointStore.Memory
  ( tvarCheckpointStore,
    ioRefCheckpointStore,
  )
where

import Control.Concurrent.STM
import Data.IORef
import Eventium.EventSubscription (CheckpointStore (..))

-- | A 'CheckpointStore' backed by a 'TVar'. Runs in 'STM'.
tvarCheckpointStore :: TVar a -> CheckpointStore STM a
tvarCheckpointStore tvar =
  CheckpointStore
    { getCheckpoint = readTVar tvar,
      saveCheckpoint = writeTVar tvar
    }

-- | A 'CheckpointStore' backed by an 'IORef'. Runs in 'IO'.
ioRefCheckpointStore :: IORef a -> CheckpointStore IO a
ioRefCheckpointStore ref =
  CheckpointStore
    { getCheckpoint = readIORef ref,
      saveCheckpoint = writeIORef ref
    }
