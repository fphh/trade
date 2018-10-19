{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Bars where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Graphics.Rendering.Chart.Easy as E

-- | Duration.
newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord, E.PlotValue)

newtype DeltaBar = DeltaBar {
  deltaBar :: Int
  } deriving (Show, Eq, Ord)

class Time t where
  type DeltaT t :: *
  add :: t -> DeltaT t -> t
  diff :: t -> t -> DeltaT t


instance Time BarNo where
  type DeltaT BarNo = DeltaBar
  add (BarNo t) (DeltaBar dt) = BarNo (t+dt)
  diff (BarNo s) (BarNo t) = DeltaBar (s-t)


instance Time UTCTime where
  type DeltaT UTCTime = NominalDiffTime
  add = flip addUTCTime
  diff = diffUTCTime
