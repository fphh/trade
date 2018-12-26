{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Delta where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)

class DeltaType ty where
  type DeltaTy ty :: *

newtype Delta a ty = Delta {
  unDelta :: a
  } deriving (Show)

-- | Continuous.
type CDelta ty = Delta Double ty

-- | Discrete.
type DDelta ty = Delta Int ty

class Add d where
  add :: DeltaTy d -> d -> d
  diff :: d -> d -> DeltaTy d


instance DeltaType UTCTime where
  type DeltaTy UTCTime = NominalDiffTime

instance Add UTCTime where
  add = addUTCTime
  diff = diffUTCTime


instance DeltaType (a, b) where
  type DeltaTy (a, b) = (DeltaTy a, DeltaTy b)


instance (Add a, Add b) => Add (a, b) where
  add (dx, dy) (x, y) = (add dx x, add dy y)
  diff (a, b) (x, y) = (diff a x, diff b y)
