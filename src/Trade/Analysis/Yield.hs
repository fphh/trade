

module Trade.Analysis.Yield where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.List as List

-- import qualified Trade.TStatistics.TradeStatistics as TS

-- import Trade.Trade.Signal (Signals(..), YieldSignal(..), curve)
import Trade.Type.Position (Position)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.NormTrade (NormTradeList(..), NormTrade(..))

-- import Trade.Type.Yield (Yield(..), forwardYield, ToYield)

{-
normTrade2stats :: NormTradeList ohlc -> TS.TradeStatistics
normTrade2stats (NormTradeList tl) =
  let f (NormTrade _ dur vs) = (dur, log (Vec.product (Vec.map unYield vs)))
  in TS.tradeStatistics (Vec.fromList (map f tl))
-}

sortTradesByPosition :: TradeList t ohlc -> Map Position (TradeList t ohlc)
sortTradesByPosition (TradeList tl) =
  let f acc t@(Trade stat _) = Map.insertWith (++) stat [t] acc 
  in fmap TradeList (List.foldl' f Map.empty tl)

sortNormTradesByPosition :: NormTradeList t -> Map Position (NormTradeList t)
sortNormTradesByPosition (NormTradeList tl) =
  let f acc t@(NormTrade stat _ _) = Map.insertWith (++) stat [t] acc 
  in fmap NormTradeList (List.foldl' f Map.empty tl)


{-
yields2stats :: YieldSignal ohlc -> TS.SampleStatistics 
yields2stats = TS.sampleStatistics . curve

sortYieldByPosition :: YieldSignal ohlc -> Map Position (YieldSignal ohlc)
sortYieldByPosition (YieldSignal ys) =
  let f acc (t, y) = Map.insertWith (++) (yieldTrade y) [(t, y)] acc 
  in fmap (YieldSignal . Vec.fromList) (List.foldl' f Map.empty ys)


data WinnersAndLoosers ohlc = WinnersAndLoosers {
  winners :: YieldSignal ohlc
  , loosers :: YieldSignal ohlc
  } deriving (Show)

classifyYield :: Map Position (YieldSignal ohlc) -> Map Position (WinnersAndLoosers ohlc)
classifyYield m =
  let p (_, TradeYield _ _ (Yield y)) = y > 1
      f (ws, ls) = WinnersAndLoosers (YieldSignal ws) (YieldSignal ls)
  in fmap (f . Vec.partition p . unYieldSignal) m


data WinnersAndLoosersStats ohlc = WinnersAndLoosersStats {
  winnerStats :: TS.SampleStatistics
  , loosersStats :: TS.SampleStatistics
  } deriving (Show)

classifiedYield2stats :: Map Position (WinnersAndLoosers ohlc) -> Map Position (WinnersAndLoosersStats ohlc)
classifiedYield2stats =
  fmap (\(WinnersAndLoosers ws ls) -> WinnersAndLoosersStats (yields2stats ws) (yields2stats ls))

data StatsLine ohlc = StatsLine {
  positionStats :: Position
  , winStats :: TS.SampleStatistics
  , looseStats :: TS.SampleStatistics
  }
  
classifiedStats2line :: Map Position (WinnersAndLoosersStats ohlc) -> [StatsLine ohlc]
classifiedStats2line m =
  let f position (WinnersAndLoosersStats ws ls) = ([StatsLine position ws ls]++)
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
  . sortYieldByPosition
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
