
module Trade.Statistics.Algorithm where

import Data.Time.Clock (NominalDiffTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Statistics.Sample as Stat

import Trade.Type.Equity (Equity(..))
import Trade.Type.Percent (Percent(..))
import Trade.Type.Price (Price(..))


class Statistics a where
  mean :: Vector a -> a
  stdDev :: Vector a -> a
  skewness :: Vector a -> Double
  kurtosis :: Vector a -> Double
  volatility :: Vector a -> Percent

  
instance Statistics Double where
  mean = Stat.mean
  stdDev = Stat.stdDev
  skewness = Stat.skewness
  kurtosis = Stat.kurtosis

  volatility xs =
    let as = Vec.zipWith (\a b -> log (a/b)) (Vec.tail xs) xs
        s = sqrt (fromIntegral (Vec.length as))
    in Percent (Stat.stdDev as * s)



instance Statistics Price where
  mean = Price . Stat.mean . Vec.map unPrice
  stdDev = Price . Stat.stdDev . Vec.map unPrice
  skewness = Stat.skewness . Vec.map unPrice
  kurtosis = Stat.kurtosis . Vec.map unPrice
  volatility = volatility . Vec.map unPrice

instance Statistics Equity where
  mean = Equity . Stat.mean . Vec.map unEquity
  stdDev = Equity . Stat.stdDev . Vec.map unEquity
  skewness = Stat.skewness . Vec.map unEquity
  kurtosis = Stat.kurtosis . Vec.map unEquity
  volatility = volatility . Vec.map unEquity

instance Statistics NominalDiffTime where
  mean = realToFrac . Stat.mean . Vec.map realToFrac
  stdDev = realToFrac . Stat.stdDev . Vec.map realToFrac
  skewness = Stat.skewness . Vec.map realToFrac
  kurtosis = Stat.kurtosis . Vec.map realToFrac
  volatility = volatility . Vec.map (realToFrac :: NominalDiffTime -> Double)
