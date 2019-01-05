{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.TradeStatistics where


import Data.Time.Clock (UTCTime, diffUTCTime)

import qualified Data.Map as Map


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec

import Text.Printf (printf)

import Trade.Type.Position (Position)
import Trade.Type.Trade (TradeList(..), ticker)

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Analysis.Yield (sortTradesByPosition)

import qualified Trade.Report.Report as Rep


data TradeStatistics = TradeStatistics {
  position :: Position
  , cnt :: !Int
  , mean :: !Double
  , stdDev :: !Double
  , tmean :: !Double
  , tstdDev :: !Double
  } deriving (Show)


tradeStatistics ::
  (Type2Double b) =>
  (ohlc -> b) -> TradeList UTCTime ohlc -> [TradeStatistics]
tradeStatistics extract tl =
  let m = sortTradesByPosition tl

      day = 60*60*24

      h v = case (Vec.head v, Vec.last v) of
              ((tx, x), (ty, y)) -> (realToFrac (ty `diffUTCTime` tx), log (type2double y / type2double x))

      f = Vec.fromList . map (h . Vec.map (fmap extract) . ticker) . unTradeList

      g st zs =
        let (ts, qs) = Vec.unzip zs
        in TradeStatistics {
          position = st
          , cnt = Vec.length qs
          , mean = Sample.mean qs
          , stdDev = Sample.stdDev qs
          , tmean = Sample.mean ts / day
          , tstdDev = Sample.stdDev ts / day
          }
      
      xs = Map.mapWithKey g (fmap f m)
      
  in Map.elems xs


stats2para :: TradeStatistics -> Rep.HtmlIO
stats2para stats =
  Rep.vtable $
  [ "position", show $ position stats]
  : ["cnt", show $ cnt stats]
  : ["mean", printf "%.4f log yield" $ mean stats]
  : ["stdDev", printf "%.4f log yield" $ stdDev stats]
  : ["duration mean", printf "%.2f days" $ tmean stats]
  : ["duration stdDev", printf "%.2f days" $ tstdDev stats]
  : []
