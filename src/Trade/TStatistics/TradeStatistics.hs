{-# LANGUAGE FlexibleContexts #-}


module Trade.TStatistics.TradeStatistics where


import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Statistics.Sample as Sample

import Text.Printf (printf)

import Trade.Type.Bars (DeltaTy)
import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Position (Position(..))

import qualified Trade.Type.Signal as Signal

import qualified Trade.Report.Table as Table
import Trade.Report.HtmlIO (HtmlIO)



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
  
  , totalDuration :: DeltaTy t
  , meanDurationPerTrade :: DeltaTy t

  {-
  , stdDevTime :: DeltaTy t
-}

  } -- deriving (Show)


sortTradesByPosition :: DeltaTradeList t ohlc -> Map Position [DeltaSignal t ohlc]
sortTradesByPosition (DeltaTradeList dtl) =
  let f acc t@(DeltaSignal _ pos _) = Map.insertWith (++) pos [t] acc 
  in List.foldl' f Map.empty dtl


deltaStatistics ::
  (Ord (Delta ohlc), Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
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
    , totalDuration = sum ts
    , meanDurationPerTrade = realToFrac (Sample.mean (Vec.fromList (map realToFrac ts)))

    , profitCount = length profits
    , maxProfit = mbe maximum profits
    , meanProfit = exp (Sample.mean (Vec.fromList (map log profits)))

    , lossCount = length losses
    , maxLoss = mbe minimum losses
    , meanLoss = exp (Sample.mean (Vec.fromList (map log losses)))

    , maxDrawdown = maxDD
    }

  
tradeStatistics ::
  (Ord (Delta ohlc), Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> Map Position (DeltaStatistics t)
tradeStatistics dtl =
  let ts = sortTradesByPosition dtl
  in fmap deltaStatistics ts


toRows ::
  Show (DeltaTy t) =>
  Map Position (DeltaStatistics t) -> [[String]]
toRows m =
  let percFmt = printf "%.6f"
      mbePercFmt = maybe "n/a" percFmt
      
      (hs, es) = unzip (Map.toList m)
      hds = "" : map show hs
      tts = "Total duration" : map (show . totalDuration) es
      mts = "Mean duration per trade" : map (show . meanDurationPerTrade) es

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
  (Show (DeltaTy t), Ord (Delta ohlc)
  , Num (DeltaTy t), Real (DeltaTy t), Fractional (DeltaTy t)) =>
  DeltaTradeList t ohlc -> HtmlIO
render dtl =
  let ts = tradeStatistics dtl
      rs = toRows ts
  in Table.table rs

