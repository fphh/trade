{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Statistics.YieldStatistics where

import Data.Time.Clock (NominalDiffTime)

import qualified Data.List as List
import qualified Data.Vector as Vec

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Trade.Type.DeltaSignal (DeltaSignal(..))
import qualified Trade.Type.DeltaSignal.Algorithm as DSA
import Trade.Type.Yield (LogYield(..), logYield2yield)

import qualified Trade.Report.Table as Table

import Trade.Report.ToReport (ToReport, toReport)

import Trade.Statistics.Statistics (Statistics(..)) -- , formatYield, formatStat)
import Trade.Statistics.Algorithm (mean, stdDev, skewness, kurtosis)

import Trade.Report.Pretty (Pretty, pretty)


data YieldStatistics ohlc = YieldStatistics {
  count :: !Int
  , maximumYield :: LogYield ohlc
  , minimumYield :: LogYield ohlc
  , maximumDuration :: LogYield ohlc
  , minimumDuration :: LogYield ohlc
  , meanYield :: Statistics NominalDiffTime Double
  , stdDevYield :: Statistics NominalDiffTime Double
  , skewnessYield :: Statistics Double Double
  , kurtosisYield :: Statistics Double Double
  }

yieldStatistics :: [LogYield ohlc] -> Maybe (YieldStatistics ohlc)
yieldStatistics [] = Nothing
yieldStatistics ys = Just $
  let (dts, zs) = unzip (map (\(LogYield dt y) -> (dt, y)) ys)
      dtsVec = Vec.fromList dts
      zsVec = Vec.fromList zs
  in YieldStatistics {
    count = length ys
    , maximumYield = List.maximumBy (comparing logYield) ys
    , minimumYield = List.minimumBy (comparing logYield) ys
    , maximumDuration = List.maximumBy (comparing logDuration) ys
    , minimumDuration = List.minimumBy (comparing logDuration) ys
    , meanYield = Statistics (mean dtsVec) (exp (mean zsVec))
    , stdDevYield = Statistics (stdDev dtsVec) (exp (stdDev zsVec))
    , skewnessYield = Statistics (skewness dtsVec) (Sample.skewness zsVec)
    , kurtosisYield = Statistics (kurtosis dtsVec) (Sample.kurtosis zsVec)
    }

yieldStatistics2table ::
  (Pretty ohlc) =>
  Maybe (YieldStatistics ohlc) -> [[String]]
yieldStatistics2table Nothing = [["", "", "n/a"]]
yieldStatistics2table (Just ys) =
  [ "No. of trades" : [pretty (count ys)]
  , []
  , [ "", "Yield", "Duration" ]
  , []
  , "Maximum yield trade" : [pretty (logYield2yield (maximumYield ys))]

  {-
  , "Minimum yield trade" : formatYield (logYield2yield (minimumYield ys))
  , []
  , "Maximum duration trade" : formatYield (logYield2yield (maximumDuration ys))
  , "Minimum duration trade" : formatYield (logYield2yield (minimumDuration ys))
  , []
  , "Mean" : formatStat (meanYield ys)
  , "Standard dev." : formatStat (stdDevYield ys)
  , "Skewness (log yield)" : formatStat (skewnessYield ys)
  , "Kurtosis (log yield)" : formatStat (kurtosisYield ys)
  -}
  ]

instance (Pretty ohlc) => ToReport (Maybe (YieldStatistics ohlc)) where
  toReport = Table.table . yieldStatistics2table

toYieldStatistics ::
  (Functor f) =>
  f [DeltaSignal ohlc] -> f (Maybe (YieldStatistics ohlc))
toYieldStatistics = fmap (yieldStatistics . map DSA.yield)

