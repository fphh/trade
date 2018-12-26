{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Bars where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Delta (Delta(..), DDelta, DeltaType, DeltaTy, Add, add, diff)
-- import Trade.Type.Scale (Scale, scale, factor)

-- | Duration.
newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord, E.PlotValue)

{-
newtype DeltaBar = DeltaBar {
  deltaBar :: Int
  } deriving (Show, Eq, Ord, Enum, Num)
-}

instance DeltaType BarNo where
  type DeltaTy BarNo = DDelta Bars

instance Add BarNo where
  add (Delta dt) (BarNo t) = BarNo (t+dt)
  diff (BarNo s) (BarNo t) = Delta (s-t)

