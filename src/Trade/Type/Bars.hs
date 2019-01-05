{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.Bars where

import qualified Graphics.Rendering.Chart.Easy as E

-- | Duration.
newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord, E.PlotValue)
