{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Statistics.TradeStatistics where


import qualified Data.Vector as Vec

import Data.Time.Clock (NominalDiffTime)

import qualified Data.List as List

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Trade.Type.DeltaSignal (DeltaSignal(..))
import qualified Trade.Type.DeltaSignal.Algorithm as DSA

import Trade.Type.Yield (LogYield(..), logYield2yield)

import Trade.Report.ToReport (ToReport, toReport)

import qualified Trade.Report.Table as Table

import Trade.Statistics.Statistics (Statistics(..), formatYield, formatStat)

import Trade.Statistics.Algorithm (mean, stdDev)

data TradeStatistics ohlc = TradeStatistics {
  maxPeak :: LogYield ohlc
  , meanPeak :: Statistics NominalDiffTime Double
  , stdDevPeak :: Statistics NominalDiffTime Double
  , maxDrawdown :: LogYield ohlc
  , meanDrawdown :: Statistics NominalDiffTime Double
  , stdDevDrawdown :: Statistics NominalDiffTime Double
  }

tradeStatistics :: [DeltaSignal ohlc] -> TradeStatistics ohlc
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
    , meanPeak = Statistics (mean maxDtsVec) (exp (Sample.mean maxZsVec))
    , stdDevPeak = Statistics (stdDev maxDtsVec) (exp (Sample.stdDev maxZsVec))
    , maxDrawdown = List.minimumBy (comparing logYield) mins
    , meanDrawdown =  Statistics (mean minDtsVec) (exp (Sample.mean minZsVec))
    , stdDevDrawdown = Statistics (stdDev minDtsVec) (exp (Sample.stdDev minZsVec))
    }

tradeStatistics2table :: TradeStatistics ohlc -> [[String]]
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


instance ToReport (TradeStatistics ohlc) where
  toReport = Table.table . tradeStatistics2table

toTradeStatistics ::
  (Functor f) =>
  f [DeltaSignal ohlc] -> f (TradeStatistics ohlc)
toTradeStatistics = fmap tradeStatistics
