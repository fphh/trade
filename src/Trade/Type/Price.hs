{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Trade.Type.Price where


import Text.Printf (printf)

import Trade.Type.Delta (Delta(..), ToDelta, toDelta)

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Pretty


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, Num, Fractional, E.PlotValue)

instance Pretty Price where
  pretty (Price x) = printf "%.2fPrc" x

instance ToDelta Price where
  toDelta (Price y0) (Price y) = Delta ((y - y0) / y0)
