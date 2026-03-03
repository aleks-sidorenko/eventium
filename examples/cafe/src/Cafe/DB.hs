{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cafe.DB
  ( openTab,
    getTabUuid,
    migrateTabEntity,
    TabEntity (..),
    TabEntityId,
    Key (..),
  )
where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Eventium
import Eventium.Store.Sqlite ()

share
  [mkPersist sqlSettings, mkMigrate "migrateTabEntity"]
  [persistLowerCase|
TabEntity sql=tabs
    projectionId UUID
    deriving Show
|]

-- | Opens a tab by inserting an entry into the tabs table and returning the
-- UUID.
openTab :: (MonadIO m) => SqlPersistT m (TabEntityId, UUID)
openTab = do
  uuid <- liftIO uuidNextRandom
  key <- insert (TabEntity uuid)
  return (key, uuid)

-- | Given the tab id, attempts to load the tab and return the UUID.
getTabUuid :: (MonadIO m) => TabEntityId -> SqlPersistT m (Maybe UUID)
getTabUuid tabId = fmap (\(TabEntity uid) -> uid) <$> get tabId
