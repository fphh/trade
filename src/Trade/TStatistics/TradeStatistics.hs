

module Trade.TStatistics.TradeStatistics where

import Data.Time.Clock (NominalDiffTime)


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Report.Report


data TradeStatistics = TradeStatistics {
  mean :: !Double
  , stdDev :: !Double
  } deriving (Show)

-- tradeStatistics :: Vector (NominalDiffTime, Double) -> TradeStatistics
tradeStatistics :: TradeList ohlc -> TradeStatistics
tradeStatistics as =
  let ts = Vec.map snd as
  in TradeStatistics {
   mean = Sample.mean ts
   , stdDev = Sample.stdDev ts
   }


stats2para :: TradeStatistics -> ReportItem
stats2para stats =
  vtable $
  ["mean", show $ mean stats]
  : ["stdDev", show $ stdDev stats]
  : []
