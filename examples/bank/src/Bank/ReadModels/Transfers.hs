{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bank.ReadModels.Transfers
  ( TransferEntity (..),
    migrateTransfer,
    transferReadModel,
    getTransfersByStatus,
  )
where

import Bank.Models
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Eventium
import Eventium.ProjectionCache.Sqlite (CheckpointName (..), sqliteCheckpointStore)

share
  [mkPersist sqlSettings {mpsFieldLabelModifier = const id}, mkMigrate "migrateTransfer"]
  [persistLowerCase|
TransferEntity sql=transfers
    transferId UUID
    sourceAccount UUID
    targetAccount UUID
    amount Double
    status Text
    createdAt UTCTime Maybe
    updatedAt UTCTime Maybe
    UniqueTransferId transferId
    deriving Show
|]

handleTransferEvent :: (MonadIO m) => GlobalStreamEvent BankEvent -> SqlPersistT m ()
handleTransferEvent globalEvent =
  case globalEvent.payload.payload of
    AccountTransferStartedEvent (AccountTransferStarted tid amt target) -> do
      let source = globalEvent.payload.key
          created = globalEvent.payload.metadata.createdAt
      insert_
        TransferEntity
          { transferId = tid,
            sourceAccount = source,
            targetAccount = target,
            amount = amt,
            status = "Pending",
            createdAt = created,
            updatedAt = created
          }
    AccountTransferCompletedEvent (AccountTransferCompleted tid) -> do
      now <- liftIO getCurrentTime
      updateWhere
        [TransferEntityTransferId ==. tid]
        [TransferEntityStatus =. ("Completed" :: Text), TransferEntityUpdatedAt =. Just now]
    AccountTransferFailedEvent (AccountTransferFailed tid _reason) -> do
      now <- liftIO getCurrentTime
      updateWhere
        [TransferEntityTransferId ==. tid]
        [TransferEntityStatus =. ("Failed" :: Text), TransferEntityUpdatedAt =. Just now]
    _ -> pure ()

getTransfersByStatus :: (MonadIO m) => Text -> SqlPersistT m [Entity TransferEntity]
getTransfersByStatus s = selectList [TransferEntityStatus ==. s] []

transferReadModel :: ReadModel (SqlPersistT IO) BankEvent
transferReadModel =
  ReadModel
    { initialize = void $ runMigrationSilent migrateTransfer,
      eventHandler = EventHandler handleTransferEvent,
      checkpointStore = sqliteCheckpointStore (CheckpointName "transfers"),
      reset = deleteWhere ([] :: [Filter TransferEntity])
    }
