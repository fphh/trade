{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.TStatistics.TradeStatistics where

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Text.Printf (printf, PrintfArg)

import Trade.Type.Bars (DeltaTy)
import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import qualified Trade.Type.DeltaSignal.Algorithm as DSA
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Position (Position(..))
import qualified Trade.Type.NestedMap as NMap 
import Trade.Type.Yield (Yield(..), LogYield(..), logYield2yield, yield)

import qualified Trade.Type.Signal as Signal

import qualified Trade.Report.Table as Table
import Trade.Report.HtmlIO (HtmlIO)
import Trade.Report.Pretty (pretty, Pretty)

import Debug.Trace


data Statistics t y = Statistics {
  statDuration :: t
  , statYield :: y
  }

formatStat :: (Pretty t, PrintfArg y) => Statistics t y -> [String]
formatStat (Statistics dt y) = [ printf "%.10f" y, pretty dt ]

formatYield :: (Pretty t) =>  Yield t ohlc -> [String]
formatYield (Yield dt y) = [ printf "%.10f" y, pretty dt ]

data TradeStatistics t ohlc = TradeStatistics {
  maxPeak :: LogYield (DeltaTy t) ohlc
  , meanPeak :: Statistics (DeltaTy t) Double
  , stdDevPeak :: Statistics (DeltaTy t) Double
  , maxDrawdown :: LogYield (DeltaTy t) ohlc
  , meanDrawdown :: Statistics (DeltaTy t) Double
  , stdDevDrawdown :: Statistics (DeltaTy t) Double
  }

tradeStatistics ::
  (Eq (DeltaTy t), Fractional (DeltaTy t), Real (DeltaTy t)) =>
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
    , meanPeak = Statistics (realToFrac (Sample.mean maxDtsVec)) (exp (Sample.mean maxZsVec))
    , stdDevPeak = Statistics (realToFrac (Sample.stdDev maxDtsVec)) (exp (Sample.stdDev maxZsVec))
    , maxDrawdown = List.minimumBy (comparing logYield) mins
    , meanDrawdown =  Statistics (realToFrac (Sample.mean minDtsVec)) (exp (Sample.mean minZsVec))
    , stdDevDrawdown = Statistics (realToFrac (Sample.stdDev minDtsVec)) (exp (Sample.stdDev minZsVec))
    }

tradeStatistics2table :: (Pretty (DeltaTy t)) => TradeStatistics t ohlc -> [[String]]
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

data YieldStatistics t ohlc = YieldStatistics {
  count :: !Int
  , maximumYield :: LogYield (DeltaTy t) ohlc
  , minimumYield :: LogYield (DeltaTy t) ohlc
  , maximumDuration :: LogYield (DeltaTy t) ohlc
  , minimumDuration :: LogYield (DeltaTy t) ohlc
  , meanYield :: Statistics (DeltaTy t) Double
  , stdDevYield :: Statistics (DeltaTy t) Double
  , skewnessYield :: Statistics Double Double
  , kurtosisYield :: Statistics Double Double
  }

yieldList2statistics ::
  (Fractional (DeltaTy t), Real (DeltaTy t)) =>
  [LogYield (DeltaTy t) ohlc] -> Maybe (YieldStatistics t ohlc)
yieldList2statistics [] = Nothing
yieldList2statistics ys = Just $
  let (dts, zs) = unzip (map (\(LogYield dt y) -> (dt, y)) ys)
      dtsVec = Vec.map realToFrac (Vec.fromList dts)
      zsVec = Vec.fromList zs
  in YieldStatistics {
    count = length ys
    , maximumYield = List.maximumBy (comparing logYield) ys
    , minimumYield = List.minimumBy (comparing logYield) ys
    , maximumDuration = List.maximumBy (comparing logDuration) ys
    , minimumDuration = List.minimumBy (comparing logDuration) ys
    , meanYield = Statistics (realToFrac (Sample.mean dtsVec)) (exp (Sample.mean zsVec))
    , stdDevYield = Statistics (realToFrac (Sample.stdDev dtsVec)) (exp (Sample.stdDev zsVec))
    , skewnessYield = Statistics (Sample.skewness dtsVec) (Sample.skewness zsVec)
    , kurtosisYield = Statistics (Sample.kurtosis dtsVec) (Sample.kurtosis zsVec)
    }

yieldStatistics2table ::
  Pretty (DeltaTy t) => Maybe (YieldStatistics t ohlc) -> [[String]]
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


renderYieldStatistics ::
  forall t ohlc.
  (Pretty (DeltaTy t), Ord (Delta ohlc)
  , Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> HtmlIO
renderYieldStatistics dtl =
  let ts = DSA.sortDeltaSignals dtl

      f pos wol xs =
        let ylds :: Maybe (YieldStatistics t ohlc)
            ylds = yieldList2statistics (map DSA.yield xs)
            ys = yieldStatistics2table ylds
            ts = tradeStatistics2table (tradeStatistics xs)
        in [[[ show pos ++ " / " ++ show wol ]
            , Table.boldSep 24 3 ]
            ++ ys
            ++ [Table.boldSep 24 3]
            ++ ts]
      
      ss = NMap.fold f ts
      
  in Table.tableList ss
