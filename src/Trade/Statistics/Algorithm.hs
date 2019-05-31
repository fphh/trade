
module Trade.Statistics.Algorithm where

import Data.Time.Clock (NominalDiffTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Statistics.Sample as Stat

import Trade.Type.Price (Price(..))

class Statistics a where
  mean :: Vector a -> a
  stdDev :: Vector a -> a
  skewness :: Vector a -> Double
  kurtosis :: Vector a -> Double

  
instance Statistics Double where
  mean = Stat.mean
  stdDev = Stat.stdDev
  skewness = Stat.skewness
  kurtosis = Stat.kurtosis

instance Statistics Price where
  mean = Price . Stat.mean . Vec.map unPrice
  stdDev = Price . Stat.stdDev . Vec.map unPrice
  skewness = Stat.skewness . Vec.map unPrice
  kurtosis = Stat.kurtosis . Vec.map unPrice

instance Statistics NominalDiffTime where
  mean = realToFrac . Stat.mean . Vec.map realToFrac
  stdDev = realToFrac . Stat.stdDev . Vec.map realToFrac
  skewness = Stat.skewness . Vec.map realToFrac
  kurtosis = Stat.kurtosis . Vec.map realToFrac
