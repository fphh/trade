{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.TStatistics.TradeStatistics where


import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Ord (comparing)

import qualified Statistics.Sample as Sample

import Text.Printf (printf)

import Trade.Type.Bars (DeltaTy)
import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaSignal.Algorithm (toYield)
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Position (Position(..))
import Trade.Type.Yield (Yield(..), LogYield(..), logYield2yield, yield)

import qualified Trade.Type.Signal as Signal

import qualified Trade.Report.Table as Table
import Trade.Report.HtmlIO (HtmlIO)
import Trade.Report.Pretty (pretty, Pretty)

import Debug.Trace

data YStatistics t y = YStatistics {
  statDuration :: t
  , statYield :: y
  }

data YieldStatistics t ohlc = YieldStatistics {
  count :: !Int
  , maximumYield :: LogYield (DeltaTy t) ohlc
  , minimumYield :: LogYield (DeltaTy t) ohlc
  , maximumDuration :: LogYield (DeltaTy t) ohlc
  , minimumDuration :: LogYield (DeltaTy t) ohlc
  , meanYield :: YStatistics (DeltaTy t) Double
  , stdDevYield :: YStatistics (DeltaTy t) Double
  , skewnessYield :: YStatistics Double Double
  , kurtosisYield :: YStatistics Double Double
  }

yieldStatistics ::
  (Fractional (DeltaTy t), Real (DeltaTy t)) =>
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
    , meanYield = YStatistics (realToFrac (Sample.mean dtsVec)) (exp (Sample.mean zsVec))
    , stdDevYield = YStatistics (realToFrac (Sample.stdDev dtsVec)) (exp (Sample.stdDev zsVec))
    , skewnessYield = YStatistics (Sample.skewness dtsVec) (Sample.skewness zsVec)
    , kurtosisYield = YStatistics (Sample.kurtosis dtsVec) (Sample.kurtosis zsVec)
    }

yieldStatistics2table ::
  forall t ohlc.
  Pretty (DeltaTy t) =>
  Position -> (Maybe (YieldStatistics t ohlc), Maybe (YieldStatistics t ohlc)) -> [[String]]
yieldStatistics2table pos (losers, winners) =
  let f (Yield dt y) = [ printf "%.10f" y, pretty dt ]
      yStat (YStatistics dt y) = [ printf "%.10f" y, pretty dt ]
      width = 24
      seperator = replicate 3 (replicate width '=')
      mbe _ Nothing = ["n/a", "n/a"]
      mbe f (Just x) = f x
      h str ys =
        [ [ str, show pos, "" ]
        , seperator
        , "No. of trades" : mbe ((:[""]) . show . count) ys
        , []
        , [ printf "%s" "", printf "%s" "Yield", printf "%s"  "Duration" ]
        , []
        , "Maximum yield trade" : mbe (f . logYield2yield . maximumYield) ys
        , "Minimum yield trade" : mbe (f . logYield2yield . minimumYield) ys
        , []
        , "Maximum duration trade" : mbe (f . logYield2yield . maximumDuration) ys
        , "Minimum duration trade" : mbe (f . logYield2yield . minimumDuration) ys
        , []
        , "Mean" : mbe (yStat . meanYield) ys
        , "Standard dev." : mbe (yStat . stdDevYield) ys
        , "Skewness of log yield" : mbe (yStat . skewnessYield) ys
        , "Kurtosis of log yield" : mbe (yStat . kurtosisYield) ys
        ]
  in h "Winning" winners
     ++ [seperator]
     ++ h "Losing" losers


    
yieldStatistics2table pos xs = [[show pos, "n/a"]]
     
sortTradesByPosition :: DeltaTradeList t ohlc -> Map Position [DeltaSignal t ohlc]
sortTradesByPosition (DeltaTradeList dtl) =
  let f acc t@(DeltaSignal _ pos _) = Map.insertWith (++) pos [t] acc 
  in List.foldl' f Map.empty dtl


tradeStatistics ::
  (Ord (Delta ohlc), Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> Map Position (Maybe (YieldStatistics t ohlc), Maybe (YieldStatistics t ohlc))
tradeStatistics dtl =
  let ts = fmap (map toYield) (sortTradesByPosition dtl)
      winnerOrLoser [] = ([], [])
      winnerOrLoser xs = List.partition ((<= 0) . logYield) xs
      us = fmap winnerOrLoser ts
  in fmap (\(a, b) -> (yieldStatistics a, yieldStatistics b)) us


render ::
  (Pretty (DeltaTy t), Ord (Delta ohlc)
  , Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> HtmlIO
render dtl =
  let ts = tradeStatistics dtl
      rs = Map.mapWithKey yieldStatistics2table ts
      [xs, ys] = Map.elems rs
  in Table.table (zipWith (++) xs ys)
