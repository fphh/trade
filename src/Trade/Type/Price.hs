{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Trade.Type.Price where


import Trade.Type.Delta (Delta(..), ToDelta, toDelta)

import qualified Graphics.Rendering.Chart.Easy as E

import Debug.Trace


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, E.PlotValue)


instance ToDelta Price where
  toDelta (Price y0) (Price y) = Delta ((y - y0) / y0)
