{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.TStatistics.SampleStatistics where


import Text.Printf (printf)

import Trade.Type.Signal (Signal(..))
import Trade.Type.Yield (LogYield(..))

import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Trade.Report.Report as Rep

import Prelude hiding (maximum, minimum)


data SampleStatistics t = SampleStatistics {
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
  , from :: t
  , startValue :: !Double
  , to :: t
  , endValue :: !Double
  } deriving (Show)

sampleStatistics' :: Vector (t, Double) -> SampleStatistics t
sampleStatistics' as =
  let ts = Vec.map snd as
  in SampleStatistics {
    mean = Sample.mean ts
    , stdDev = Sample.stdDev ts
    , variance = Sample.variance ts
    , stdErrMean = Sample.stdErrMean ts
    , skewness = Sample.skewness ts
    , kurtosis = Sample.kurtosis ts
    -- , autocorrelation = Sample.kurtosis ts -- ???
    , minimum = Vec.minimum ts
    , maximum = Vec.maximum ts
    , range = Sample.range ts
    , count = Vec.length ts
    , from = fst (Vec.head as)
    , startValue = snd (Vec.head as)
    , to = fst (Vec.last as)
    , endValue = snd (Vec.last as)
    }


class SampleStats a where
  type SampleStatsTy a :: *
  sampleStatistics :: a -> SampleStatistics (SampleStatsTy a)

instance SampleStats (Vector (t, Double)) where
  type SampleStatsTy (Vector (t, Double)) = t
  sampleStatistics = sampleStatistics'

instance SampleStats (Signal t Double) where
  type SampleStatsTy (Signal t Double) = t
  sampleStatistics = sampleStatistics' . unSignal

instance SampleStats (Signal t LogYield) where
  type SampleStatsTy (Signal t LogYield) = t
  sampleStatistics = sampleStatistics' . Vec.map (\(a, LogYield y) -> (a, y)) . unSignal

stats2para :: (Show a) => SampleStatistics a -> Rep.HtmlIO
stats2para stats =
  Rep.vtable $
  ["mean", show $ mean stats]
  : ["stdDev", show $ stdDev stats]
  : ["variance", show $ variance stats]
  : ["stdErrMean", show $ stdErrMean stats]
  : ["skewness", show $ skewness stats]
  : ["kurtosis", show $ kurtosis stats]
  -- : ["max profit", show (maximum stats / minimum stats)]
  : ["range", show $ range stats]
  : ["count", show $ count stats]
  : ["from", show $ from stats]
  : ["startValue", printf "%.2f"  $ startValue stats]
  : ["to", show $ to stats]
  : ["endValue", printf "%.2f"  $ endValue stats]
  : ["minimum", printf "%.2f" $ minimum stats]
  : ["maximum", printf "%.2f" $ maximum stats]
  : []
