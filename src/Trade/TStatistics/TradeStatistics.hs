{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.TStatistics.TradeStatistics where


import qualified Data.Vector as Vec

import qualified Data.List as List

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Trade.Type.Bars (DeltaTy)
import Trade.Type.DeltaSignal (DeltaSignal(..))
import qualified Trade.Type.DeltaSignal.Algorithm as DSA

--import qualified Trade.Type.NestedMap as NMap
--import Trade.Type.NestedMap (NestedMap)

import Trade.Type.Yield (LogYield(..), logYield2yield)

import Trade.Report.HtmlIO (ToHtmlIO, toHtmlIO)
import Trade.Report.Pretty (Pretty)

import Trade.TStatistics.Statistics (Statistics(..), DeltaTyStats(..), formatYield, formatStat)


data TradeStatistics t ohlc = TradeStatistics {
  maxPeak :: LogYield (DeltaTy t) ohlc
  , meanPeak :: Statistics (DeltaTyStats t) Double
  , stdDevPeak :: Statistics (DeltaTyStats t) Double
  , maxDrawdown :: LogYield (DeltaTy t) ohlc
  , meanDrawdown :: Statistics (DeltaTyStats t) Double
  , stdDevDrawdown :: Statistics (DeltaTyStats t) Double
  }

tradeStatistics ::
  (Eq (DeltaTy t), Real (DeltaTy t)) =>
  [DeltaSignal t ohlc] -> TradeStatistics t ohlc
tradeStatistics dts =
  let maxs = map DSA.maximum dts
      mins = map DSA.minimum dts
      (maxDts, maxZs) = unzip (map (\(LogYield dt y) -> (dt, y)) maxs)
      maxDtsVec = Vec.map realToFrac (Vec.fromList maxDts)
      maxZsVec = Vec.fromList maxZs
      (minDts, minZs) = unzip (map (\(LogYield dt y) -> (dt, y)) mins)
      minDtsVec = Vec.map realToFrac (Vec.fromList minDts)
      minZsVec = Vec.fromList minZs
  in TradeStatistics {
    maxPeak = List.maximumBy (comparing logYield) maxs
    , meanPeak = Statistics (DeltaTyStats (Sample.mean maxDtsVec)) (exp (Sample.mean maxZsVec))
    , stdDevPeak = Statistics (DeltaTyStats (Sample.stdDev maxDtsVec)) (exp (Sample.stdDev maxZsVec))
    , maxDrawdown = List.minimumBy (comparing logYield) mins
    , meanDrawdown =  Statistics (DeltaTyStats (Sample.mean minDtsVec)) (exp (Sample.mean minZsVec))
    , stdDevDrawdown = Statistics (DeltaTyStats (Sample.stdDev minDtsVec)) (exp (Sample.stdDev minZsVec))
    }

tradeStatistics2table :: (Pretty (DeltaTy t), Pretty (DeltaTyStats t)) => TradeStatistics t ohlc -> [[String]]
tradeStatistics2table ts =
  [ ["Trade Statistics", "Yield", "Dur. from trade start"]
  , []
  , "Max. peak" : formatYield (logYield2yield (maxPeak ts))
  , "Mean peak" : formatStat (meanPeak ts)
  , "StdDev peak" : formatStat (stdDevPeak ts)
  , []
  , "Max. drawdown" : formatYield (logYield2yield (maxDrawdown ts))
  , "Mean drawdown" : formatStat (meanDrawdown ts)
  , "StdDev drawdown" : formatStat (stdDevDrawdown ts)
  ]

{-
toTradeStatistics ::
  (Functor f, Real (DeltaTy t)) =>
  f [DeltaSignal t ohlc] -> f (YieldStatistics t ohlc)
toTradeStatistics = fmap (yieldList2statistics . map DSA.yield)
-}
