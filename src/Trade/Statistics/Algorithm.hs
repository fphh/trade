
module Trade.Statistics.Algorithm where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Statistics.Sample as Stat

import Trade.Type.Price (Price(..))

class Statistics a where
  mean :: Vector a -> a
  stdDev :: Vector a -> a

instance Statistics Price where
  mean = Price . Stat.mean . Vec.map unPrice
  stdDev = Price . Stat.stdDev . Vec.map unPrice
