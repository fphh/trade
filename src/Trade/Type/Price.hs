{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Trade.Type.Price where


import Trade.Type.Delta (Delta(..), ToDelta, toDelta)

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import qualified Graphics.Rendering.Chart.Easy as E


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, E.PlotValue)

instance ToDelta Price where
  toDelta (Price y0) (Price y) = Delta ((y - y0) / y0)

instance Type2Double Price where
  type2double = unPrice
