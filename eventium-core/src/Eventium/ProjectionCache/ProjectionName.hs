{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eventium.ProjectionCache.ProjectionName
  ( ProjectionName (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

-- | A name identifying a projection in the projection cache.
-- Used as a discriminator so multiple projections can share one storage table.
newtype ProjectionName = ProjectionName Text
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON)
