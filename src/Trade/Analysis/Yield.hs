

module Trade.Analysis.Yield where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Trade.TStatistics.SampleStatistics as TS

-- import Trade.Trade.Signal (Signals(..), YieldSignal(..), curve)
import Trade.Trade.State (State)
import Trade.Trade.TradeSignal

import Trade.Type.Yield (Yield(..))

import Trade.Report.Report (ReportItem, subheader, htable)


trade2stats :: TradeList ohlc -> TS.SampleStatistics
trade2stats (TradeList tl) =
  let f (t0, y0) (_, y1) = (t0, unYield (y1 `forwardYield` y0))
  in TS.sampleStatistics (map f tl)

{-
yields2stats :: YieldSignal ohlc -> TS.SampleStatistics 
yields2stats = TS.sampleStatistics . curve

sortYieldByState :: YieldSignal ohlc -> Map State (YieldSignal ohlc)
sortYieldByState (YieldSignal ys) =
  let f acc (t, y) = Map.insertWith (++) (yieldTrade y) [(t, y)] acc 
  in fmap (YieldSignal . Vec.fromList) (List.foldl' f Map.empty ys)


data WinnersAndLoosers ohlc = WinnersAndLoosers {
  winners :: YieldSignal ohlc
  , loosers :: YieldSignal ohlc
  } deriving (Show)

classifyYield :: Map State (YieldSignal ohlc) -> Map State (WinnersAndLoosers ohlc)
classifyYield m =
  let p (_, TradeYield _ _ (Yield y)) = y > 1
      f (ws, ls) = WinnersAndLoosers (YieldSignal ws) (YieldSignal ls)
  in fmap (f . Vec.partition p . unYieldSignal) m


data WinnersAndLoosersStats ohlc = WinnersAndLoosersStats {
  winnerStats :: TS.SampleStatistics
  , loosersStats :: TS.SampleStatistics
  } deriving (Show)

classifiedYield2stats :: Map State (WinnersAndLoosers ohlc) -> Map State (WinnersAndLoosersStats ohlc)
classifiedYield2stats =
  fmap (\(WinnersAndLoosers ws ls) -> WinnersAndLoosersStats (yields2stats ws) (yields2stats ls))

data StatsLine ohlc = StatsLine {
  stateStats :: State
  , winStats :: TS.SampleStatistics
  , looseStats :: TS.SampleStatistics
  }
  
classifiedStats2line :: Map State (WinnersAndLoosersStats ohlc) -> [StatsLine ohlc]
classifiedStats2line m =
  let f state (WinnersAndLoosersStats ws ls) = ([StatsLine state ws ls]++)
  in Map.foldrWithKey' f [] m

lines2para :: [StatsLine ohlc] -> [ReportItem]
lines2para =
  let f (StatsLine st ws ls) = [subheader (show st), TS.stats2para ws, TS.stats2para ls]
  in concatMap f

lines2table :: (Show a) => (TS.SampleStatistics -> a) -> [StatsLine ohlc] -> ReportItem
lines2table f ls =
  let g (StatsLine st ws ls) = [ show st, show (f ws), show (f ls) ]  
  in htable (["\\", "Winners", "Loosers"] : map g ls)

yieldStats2lines :: Signals ohlc -> [StatsLine ohlc]
yieldStats2lines =
  classifiedStats2line
  . classifiedYield2stats
  . classifyYield
  . sortYieldByState
  . yields

yieldStats :: Signals ohlc -> [ReportItem]
yieldStats =
  lines2para
  . yieldStats2lines

yieldStatsTable :: (Show a) => (TS.SampleStatistics -> a) -> Signals ohlc -> ReportItem
yieldStatsTable f =
  lines2table f
  . yieldStats2lines
  
-}
