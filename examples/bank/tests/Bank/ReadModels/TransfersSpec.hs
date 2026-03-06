{-# LANGUAGE OverloadedStrings #-}

module Bank.ReadModels.TransfersSpec (spec) where

import Bank.Models
import Bank.ReadModels.Transfers
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Eventium
import Eventium.ProjectionCache.Sqlite (initializeSqliteProjectionCache)
import Eventium.Store.Sqlite
import Test.Hspec

spec :: Spec
spec = do
  describe "Transfer ReadModel" $ do
    it "should track transfer lifecycle through events" $ do
      pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
      initializeSqliteEventStore defaultSqlEventStoreConfig pool
      initializeSqliteProjectionCache pool

      let writer = codecEventStoreWriter jsonStringCodec $ sqliteEventStoreWriter defaultSqlEventStoreConfig
          globalReader = codecGlobalEventStoreReader jsonStringCodec $ sqlGlobalEventStoreReader defaultSqlEventStoreConfig
          rm = transferReadModel

      flip runSqlPool pool $ do
        rm.initialize

        let sourceId = uuidFromInteger 1
            targetId = uuidFromInteger 2
            transferUuid = uuidFromInteger 100

        _ <-
          writer.storeEvents
            sourceId
            NoStream
            [AccountTransferStartedEvent (AccountTransferStarted transferUuid 50.0 targetId)]

        _ <-
          writer.storeEvents
            targetId
            NoStream
            [AccountCreditedFromTransferEvent (AccountCreditedFromTransfer transferUuid sourceId 50.0)]

        _ <-
          writer.storeEvents
            sourceId
            (ExactPosition 0)
            [AccountTransferCompletedEvent (AccountTransferCompleted transferUuid)]

        pure ()

      runSqlPool (rebuildReadModel globalReader rm) pool

      transfers <- runSqlPool (getTransfersByStatus "Completed") pool
      length transfers `shouldBe` 1

    it "should track failed transfers" $ do
      pool <- runNoLoggingT (createSqlitePool ":memory:" 1)
      initializeSqliteEventStore defaultSqlEventStoreConfig pool
      initializeSqliteProjectionCache pool

      let writer = codecEventStoreWriter jsonStringCodec $ sqliteEventStoreWriter defaultSqlEventStoreConfig
          globalReader = codecGlobalEventStoreReader jsonStringCodec $ sqlGlobalEventStoreReader defaultSqlEventStoreConfig
          rm = transferReadModel

      flip runSqlPool pool $ do
        rm.initialize

        let sourceId = uuidFromInteger 1
            targetId = uuidFromInteger 2
            transferUuid = uuidFromInteger 200

        _ <-
          writer.storeEvents
            sourceId
            NoStream
            [AccountTransferStartedEvent (AccountTransferStarted transferUuid 50.0 targetId)]

        _ <-
          writer.storeEvents
            sourceId
            (ExactPosition 0)
            [AccountTransferFailedEvent (AccountTransferFailed transferUuid "Insufficient funds")]

        pure ()

      runSqlPool (rebuildReadModel globalReader rm) pool

      pending' <- runSqlPool (getTransfersByStatus "Pending") pool
      length pending' `shouldBe` 0

      failed <- runSqlPool (getTransfersByStatus "Failed") pool
      length failed `shouldBe` 1
