{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Trade.Type.Price where


import Text.Printf (printf)

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Delta (Delta(..), ToDelta, toDelta)
import Trade.Type.Scale (Scale, scale)
import Trade.Type.Add (Add, add)


import Trade.Report.Pretty


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, Num, Fractional, Floating, E.PlotValue)

instance Pretty Price where
  pretty (Price x) = printf "%.2fPrc" x

instance ToDelta Price where
  toDelta (Price y0) (Price y) = Delta ((y - y0) / y0)


instance Scale Price where
  scale x (Price p) = Price (x*p)

instance Add Price where
  add (Price p) (Price q) = Price (p+q)
