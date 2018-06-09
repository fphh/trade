{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.TradeStatistics where


import qualified Data.Map as Map


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Analysis.Yield

import Trade.Type.EquityAndShare

import Trade.Report.Report


data TradeStatistics = TradeStatistics {
  state :: State
  , cnt :: !Int
  , mean :: !Double
  , stdDev :: !Double
  } deriving (Show)


-- tradeStatistics :: () -> TradeList ohlc -> [TradeStatistics]
tradeStatistics :: (UnOHLC b) => (ohlc -> b) -> TradeList ohlc -> [TradeStatistics]
tradeStatistics extract tl =
  let m = sortTradeByState tl

      h v = case (Vec.head v, Vec.last v) of
              ((_, x), (_, y)) -> unOHLC y / unOHLC x

      f = Vec.fromList . map (log . h . Vec.map (fmap extract) . ticker) . unTradeList

      g st ts = TradeStatistics {
        state = st
        , cnt = Vec.length ts
        , mean = Sample.mean ts
        , stdDev = Sample.stdDev ts
        }
      
      xs = Map.mapWithKey g (fmap f m)
      
  in Map.elems xs


stats2para :: TradeStatistics -> ReportItem
stats2para stats =
  vtable $
  [ "state", show $ state stats]
  : ["cnt", show $ cnt stats]
  : ["mean", show $ mean stats]
  : ["stdDev", show $ stdDev stats]
  : []
