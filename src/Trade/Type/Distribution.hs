

module Trade.Type.Distribution where

import Data.Vector (Vector)

type Percent = Double

newtype CDF a = CDF {
  unCDF :: Vector (Percent, Double)
  } deriving (Show)


{-
data Distribution = Distribution {
  twrCDF :: [(Fraction, CDF TWR)]
  , riskCDF :: [(Fraction, CDF Risk)]
  } deriving (Show)
-}
