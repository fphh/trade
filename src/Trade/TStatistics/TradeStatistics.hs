{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.TradeStatistics where

import Data.Time.Clock (NominalDiffTime)

import qualified Data.Map as Map


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Analysis.Yield

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Report.Report


data TradeStatistics = TradeStatistics {
  state :: State
  , mean :: !Double
  , stdDev :: !Double
  } deriving (Show)


tradeStatistics :: TradeList Close -> [TradeStatistics]
tradeStatistics tl =
  let m = sortTradeByState tl

      h v = case (Vec.head v, Vec.last v) of
              ((_, Close x), (_, Close y)) -> y/x
              
      f = Vec.fromList . map (log . h . ticker) . unTradeList

      g st ts = TradeStatistics {
        state = st
        , mean = Sample.mean ts
        , stdDev = Sample.stdDev ts
        }
      
      xs = Map.mapWithKey g (fmap f m)
      
  in Map.elems xs


stats2para :: TradeStatistics -> ReportItem
stats2para stats =
  vtable $
  [ "state", show $ state stats]
  : ["mean", show $ mean stats]
  : ["stdDev", show $ stdDev stats]
  : []
