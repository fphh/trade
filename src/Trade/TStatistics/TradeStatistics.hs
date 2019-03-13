{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.TradeStatistics where

import Text.Printf (printf)

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Statistics.Sample as Sample

import Trade.Type.Bars (DeltaTy) -- , FormatDelta, formatDelta)
import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Position (Position(..))

import qualified Trade.Type.Signal as Signal

import qualified Trade.Report.Table as Table
import Trade.Report.HtmlIO (HtmlIO)

import Debug.Trace



data DeltaStatistics t = DeltaStatistics {
  numberOfTrades :: !Int

  , profitCount :: !Int
  , maxProfit :: Maybe Double
  , meanProfit :: !Double
  {-
  , stdDevProfit :: !Double
-}
  , lossCount :: !Int
  , maxLoss :: Maybe Double
  , meanLoss :: !Double

{-
  , stdDevLoss :: !Double
-}
  , maxDrawdown :: Maybe Double
{-
  , meanDrawdown :: !Double
  , stdDevDrawdown :: !Double
-}
  
  , totalTime :: DeltaTy t
  , meanDuration :: DeltaTy t

  {-
  , stdDevTime :: DeltaTy t
-}

  } -- deriving (Show)


sortTradesByPosition :: DeltaTradeList t ohlc -> Map Position [DeltaSignal t ohlc]
sortTradesByPosition (DeltaTradeList dtl) =
  let f acc t@(DeltaSignal _ pos _) = Map.insertWith (++) pos [t] acc 
  in List.foldl' f Map.empty dtl


deltaStatistics ::
  (Num (DeltaTy t), Ord (Delta ohlc), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  [DeltaSignal t ohlc] -> DeltaStatistics t
deltaStatistics xs =
  let nots = length xs
      f (DeltaSignal _ _ as) = fmap ((1+) . unDelta) (Signal.last as)
      (ts, ys) = unzip (map f xs)
      
      profits = filter (>=1) ys
      losses = filter (<1) ys

      mbe _ [] = Nothing
      mbe k as = Just (k as)

      g (DeltaSignal _ _ as) = Vec.minimum (Vec.map ((1+) . unDelta . snd) (Signal.unSignal as))
      maxDD = mbe (minimum . map g) xs
 

  in DeltaStatistics {
    numberOfTrades = nots
    , totalTime = sum ts
    , meanDuration = realToFrac (Sample.mean (Vec.fromList (map realToFrac ts)))

    , profitCount = length profits
    , maxProfit = mbe maximum profits
    , meanProfit = exp (Sample.mean (Vec.fromList (map log profits)))

    , lossCount = length losses
    , maxLoss = mbe minimum losses
    , meanLoss = exp (Sample.mean (Vec.fromList (map log losses)))

    , maxDrawdown = maxDD
    }

  
tradeStatistics ::
  (Num (DeltaTy t), Ord (Delta ohlc), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> Map Position (DeltaStatistics t)
tradeStatistics dtl =
  let ts = sortTradesByPosition dtl
  in fmap deltaStatistics ts


toRows ::
  (Show (DeltaTy t)) => -- , FormatDelta t) =>
  Map Position (DeltaStatistics t) -> [[String]]
toRows m =
  let percFmt = printf "%.6f"
      mbePercFmt = maybe "n/a" percFmt
      
      (hs, es) = unzip (Map.toList m)
      hds = "" : map show hs
      -- tts = "Total time" : map (formatDelta . totalTime) es
      tts = "Total time" : map (show . totalTime) es
      -- mts = "Mean duration" : map (printf "%.2fd" . (/ (24*60*60)) . meanDuration) es
      mts = "Mean duration" : map (show . meanDuration) es

      nots = "Number of trades" : map (show . numberOfTrades) es
      
      profCnt = "Number of winning trades" : map (show . profitCount) es
      maxProf = "Max. profit" : map (mbePercFmt . maxProfit) es
      meanProf = "Mean profit" : map (percFmt . meanProfit) es

      lossCnt = "Number of losing trades" : map (show . lossCount) es
      maxLss = "Max. loss" : map (mbePercFmt . maxLoss) es
      meanLss = "Mean loss" : map (percFmt . meanLoss) es

      mdds = "Max. drawdown" : map (mbePercFmt . maxDrawdown) es
  in [ hds
     , []
     , nots
     , [], ["Time"], []
     , tts
     , mts
     , [], ["Profit"], []
     , profCnt
     , maxProf
     , meanProf
     , [], ["Loss"], []
     , lossCnt
     , maxLss
     , meanLss
     , [], ["Drawdown"], []
     , mdds]

render ::
  (Num (DeltaTy t), Show (DeltaTy t), {- FormatDelta t, -} Ord (Delta ohlc), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> HtmlIO
render dtl =
  let ts = tradeStatistics dtl
      rs = toRows ts
  in Table.table rs


  

{-
import Data.Time.Clock (UTCTime, diffUTCTime)

import qualified Data.Map as Map


import qualified Statistics.Sample as Sample

import qualified Data.Vector as Vec

import Text.Printf (printf)

import Trade.Type.Position (Position)
import Trade.Type.Trade (TradeList(..), ticker)

import Trade.Analysis.Yield (sortTradesByPosition)

import qualified Trade.Report.Report as Rep

import Trade.Report.HtmlIO (HtmlIO)
-}

{-
data TradeStatistics = TradeStatistics {
  position :: Position
  , cnt :: !Int
  , mean :: !Double
  , stdDev :: !Double
  , tmean :: !Double
  , tstdDev :: !Double
  } deriving (Show)
-}
{-
tradeStatistics ::
  (ohlc -> b) -> TradeList stgy UTCTime ohlc -> [TradeStatistics]
tradeStatistics extract tl =
  let m = sortTradesByPosition tl

      day = 60*60*24

      h v = case (Vec.head v, Vec.last v) of
              ((tx, x), (ty, y)) -> (realToFrac (ty `diffUTCTime` tx), error "tradeStatistics") -- log (type2double y / type2double x))

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


stats2para :: TradeStatistics -> HtmlIO
stats2para stats =
  Rep.vtable $
  [ "position", show $ position stats]
  : ["cnt", show $ cnt stats]
  : ["mean", printf "%.4f log yield" $ mean stats]
  : ["stdDev", printf "%.4f log yield" $ stdDev stats]
  : ["duration mean", printf "%.2f days" $ tmean stats]
  : ["duration stdDev", printf "%.2f days" $ tstdDev stats]
  : []

-}
