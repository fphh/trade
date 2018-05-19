

module Trade.TStatistics.SampleStatistics where


import Text.Printf (printf)

import Data.Time.Clock (UTCTime)


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Report.Report

import Prelude hiding (maximum, minimum)


data SampleStatistics = SampleStatistics {
  mean :: !Double
  , stdDev :: !Double
  , variance :: !Double
  , stdErrMean :: !Double
  , skewness :: !Double
  , kurtosis :: !Double
  -- , autocorrelation :: !Double
  , minimum :: !Double
  , maximum :: !Double
  , range :: !Double
  , count :: !Int
  , from :: !UTCTime
  , startValue :: !Double
  , to :: !UTCTime
  , endValue :: !Double
  } deriving (Show)

sampleStatistics :: Vector (UTCTime, Double) -> SampleStatistics
sampleStatistics as =
  let ts = Vec.map snd as
  in SampleStatistics {
    mean = Sample.mean ts
    , stdDev = Sample.stdDev ts
    , variance = Sample.variance ts
    , stdErrMean = Sample.stdErrMean ts
    , skewness = Sample.skewness ts
    , kurtosis = Sample.kurtosis ts
    -- , autocorrelation = Sample.kurtosis ts
    , minimum = Vec.minimum ts
    , maximum = Vec.maximum ts
    , range = Sample.range ts
    , count = Vec.length ts
    , from = fst (Vec.head as)
    , startValue = snd (Vec.head as)
    , to = fst (Vec.last as)
    , endValue = snd (Vec.last as)
    }


stats2para :: SampleStatistics -> ReportItem
stats2para stats =
  vtable $
  ["mean", show $ mean stats]
  : ["stdDev", show $ stdDev stats]
  : ["variance", show $ variance stats]
  : ["stdErrMean", show $ stdErrMean stats]
  : ["skewness", show $ skewness stats]
  : ["kurtosis", show $ kurtosis stats]
  : ["max profit", show (maximum stats / minimum stats)]
  : ["range", show $ range stats]
  : ["count", show $ count stats]
  : ["from", show $ from stats]
  : ["startValue", printf "%.2f"  $ startValue stats]
  : ["to", show $ to stats]
  : ["endValue", printf "%.2f"  $ endValue stats]
  : ["minimum", printf "%.2f" $ minimum stats]
  : ["maximum", printf "%.2f" $ maximum stats]
  : []


  
