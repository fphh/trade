{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.YieldStatistics where


import qualified Data.List as List
import qualified Data.Vector as Vec

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Trade.Type.Bars (DeltaTy)
import Trade.Type.DeltaSignal (DeltaSignal(..))
import qualified Trade.Type.DeltaSignal.Algorithm as DSA
import Trade.Type.Yield (LogYield(..), logYield2yield)

-- import Trade.Report.HtmlIO (ToHtmlIO, toHtmlIO)
import Trade.Report.Pretty (Pretty)
import qualified Trade.Report.Table as Table

import Trade.Analysis.ToReport (ToReport, toReport)

import Trade.TStatistics.Statistics (Statistics(..), DeltaTyStats(..), formatYield, formatStat)



data YieldStatistics t ohlc = YieldStatistics {
  count :: !Int
  , maximumYield :: LogYield (DeltaTy t) ohlc
  , minimumYield :: LogYield (DeltaTy t) ohlc
  , maximumDuration :: LogYield (DeltaTy t) ohlc
  , minimumDuration :: LogYield (DeltaTy t) ohlc
  , meanYield :: Statistics (DeltaTyStats t) Double
  , stdDevYield :: Statistics (DeltaTyStats t) Double
  , skewnessYield :: Statistics Double Double
  , kurtosisYield :: Statistics Double Double
  }

yieldStatistics ::
  (Real (DeltaTy t)) =>
  [LogYield (DeltaTy t) ohlc] -> Maybe (YieldStatistics t ohlc)
yieldStatistics [] = Nothing
yieldStatistics ys = Just $
  let (dts, zs) = unzip (map (\(LogYield dt y) -> (dt, y)) ys)
      dtsVec = Vec.map realToFrac (Vec.fromList dts)
      zsVec = Vec.fromList zs
  in YieldStatistics {
    count = length ys
    , maximumYield = List.maximumBy (comparing logYield) ys
    , minimumYield = List.minimumBy (comparing logYield) ys
    , maximumDuration = List.maximumBy (comparing logDuration) ys
    , minimumDuration = List.minimumBy (comparing logDuration) ys
    , meanYield = Statistics (DeltaTyStats (Sample.mean dtsVec)) (exp (Sample.mean zsVec))
    , stdDevYield = Statistics (DeltaTyStats (Sample.stdDev dtsVec)) (exp (Sample.stdDev zsVec))
    , skewnessYield = Statistics (Sample.skewness dtsVec) (Sample.skewness zsVec)
    , kurtosisYield = Statistics (Sample.kurtosis dtsVec) (Sample.kurtosis zsVec)
    }

yieldStatistics2table ::
  (Pretty (DeltaTy t), Pretty (DeltaTyStats t)) => Maybe (YieldStatistics t ohlc) -> [[String]]
yieldStatistics2table Nothing = [["", "", "n/a"]]
yieldStatistics2table (Just ys) =
  [ "No. of trades" : [show (count ys)]
  , []
  , [ "", "Yield", "Duration" ]
  , []
  , "Maximum yield trade" : formatYield (logYield2yield (maximumYield ys))
  , "Minimum yield trade" : formatYield (logYield2yield (minimumYield ys))
  , []
  , "Maximum duration trade" : formatYield (logYield2yield (maximumDuration ys))
  , "Minimum duration trade" : formatYield (logYield2yield (minimumDuration ys))
  , []
  , "Mean" : formatStat (meanYield ys)
  , "Standard dev." : formatStat (stdDevYield ys)
  , "Skewness (log yield)" : formatStat (skewnessYield ys)
  , "Kurtosis (log yield)" : formatStat (kurtosisYield ys)
  ]

instance (Pretty (DeltaTy t), Pretty (DeltaTyStats t)) => ToReport (Maybe (YieldStatistics t ohlc)) where
  toReport = Table.table . yieldStatistics2table

{-
instance (Pretty (DeltaTy t), Pretty (DeltaTyStats t)) => ToHtmlIO (Maybe (YieldStatistics t ohlc)) where
  toHtmlIO = Table.table . yieldStatistics2table
-}

toYieldStatistics ::
  (Functor f, Real (DeltaTy t)) =>
  f [DeltaSignal t ohlc] -> f (Maybe (YieldStatistics t ohlc))
toYieldStatistics = fmap (yieldStatistics . map DSA.yield)

