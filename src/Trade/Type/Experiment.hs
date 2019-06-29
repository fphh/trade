{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where


import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time.Clock (UTCTime)


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Graphics.Rendering.Chart.Axis.Types (PlotValue)


import Trade.Type.BarLength (BarLength)
import Trade.Type.Delta (ToDelta)

import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals, sortDeltaSignals)

import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Impulse (Impulse)
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))

import qualified Trade.Type.NestedMap as NestedMap
import Trade.Type.NestedMap (NestedMap(..))

import Trade.Type.Sample (SplitIndex(..), splitTime)

import Trade.Type.Position (Position(..))
import Trade.Type.WinningLosing (WinningLosing(..))

import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal (Timeseries)

import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)

import Trade.Type.Strategy.Index (Index(..))

import Trade.Type.Yield (ToYield)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.Invest2Impulse (Invest2Impulse, invest2impulse)

import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import Trade.Strategy.Type (AlignedSignals(..))
import qualified Trade.Strategy.Process as Strategy

import Trade.Report.Line (Line(..))


import Trade.Report.Basic (header, subheader, subsubheader)
import qualified Trade.Report.Chart as Chart
import qualified Trade.Report.SparkLine as Spark

import qualified Trade.Statistics.SampleStatistics as SS
import qualified Trade.Statistics.TradeStatistics as TS
import qualified Trade.Statistics.YieldStatistics as YS
import qualified Trade.Statistics.Summary as Sum
import Trade.Statistics.Summary (Summary)
import Trade.Statistics.Algorithm (Statistics)

import Trade.Report.ToReport (toReport)

import Trade.Report.Config (HtmlReader)
import Trade.Report.Pretty (Pretty)




data Input stgy sym ohlc = Input {
  step :: StepTy stgy
  , initialEquity :: Equity
  , barLength :: BarLength
  , symbol :: sym
  , impulseGenerator :: OptimizedImpulseGenerator ohlc
  , inputSignals :: Map sym (Timeseries ohlc)
  }

data OutputPerSymbol stgy ohlc = OutputPerSymbol {
  impulseSignal :: ImpulseSignal stgy
  , deltaTradeList :: DeltaTradeList ohlc
  , outputSignal :: Timeseries Equity
  , sortedTrades :: NestedMap Position WinningLosing [DeltaSignal ohlc]
  , summary :: Maybe Summary
  }

data Output stgy sym ohlc = Output {
  alignedSignals :: AlignedSignals sym ohlc
  , outputPerSymbol :: Map sym (OutputPerSymbol stgy ohlc)
  }


data Result stgy sym ohlc = Result {
  input :: Input stgy sym ohlc
  , output :: Output stgy sym ohlc
  }


conduct ::
  forall stgy sym ohlc.
  ( Ord sym
  , Show ohlc
  , ToDelta ohlc
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , Invest2Impulse stgy
  , StepFunction (StepTy stgy)) =>
  Input stgy sym ohlc -> Result stgy sym ohlc
 
conduct inp@(Input stp eqty _ sym impGen ps) =
  
  let ((asigs, stgy), _) = Strategy.run sym ps impGen
      timeLine = alignedTimes asigs

      f smbl isig =
        let impSig = invest2impulse (stgy Map.! smbl)
            ts = impulse2tradeList isig impSig
            dts = tradeList2DeltaTradeList ts
            sds = sortDeltaSignals dts
            sumry = Sum.toSummary sds
            outSig = Signal.adjust eqty timeLine (concatDeltaSignals stp eqty dts)
        in OutputPerSymbol {
          impulseSignal = impSig
          , deltaTradeList = dts
          , outputSignal = outSig
          , sortedTrades = sds
          , summary = sumry
          }

      out = Output {
        alignedSignals = asigs
        , outputPerSymbol = Map.mapWithKey f (inputSignals inp)
        }
      
  in Result {
    input = inp
    , output = out
    }


tradeStatistics ::
  ( StepFunction (StepTy stgy)
  , Pretty ohlc) =>
  StepTy stgy -> NestedMap Position WinningLosing [DeltaSignal ohlc] -> HtmlReader ()
tradeStatistics stp sts = do

  let sparks = Spark.toSparkLine stp sts
      ystats = YS.toYieldStatistics sts
      tstats = TS.toTradeStatistics sts

      f _ _ ys ts sp = do
        toReport ys
        toReport ts
        toReport (sequence_ sp)

      zs = NestedMap.zipWith3 f ystats tstats sparks

      g pos wl table =
        let sty = H5A.style (H5.stringValue "clear:both;margin:18px;padding-top:24px;color:#006600")
            hdr = toReport ((H5.div ! sty) (H5.b (H5.preEscapedToHtml (show pos ++ "/" ++ show wl))))
        in [hdr, table]
  
  sequence_ (NestedMap.fold g zs)


lastEquities :: Result stgy sym ohlc -> Map sym Equity
lastEquities (Result _ out) = fmap (snd . Signal.last . outputSignal) (outputPerSymbol out)


render ::
  forall stgy ohlc sym.
  ( Show sym
  , Ord sym
  , StepFunction (StepTy stgy)
  , Floating ohlc
  , Statistics ohlc
  , ToYield ohlc
  , Pretty ohlc
  , PlotValue ohlc
  , Line (Timeseries ohlc)
  , Line (Vec.Vector (UTCTime, ohlc))) =>
  (sym -> HtmlReader ()) -> Result stgy sym ohlc -> HtmlReader ()

render addendum (Result inp out) = do

  let ops = outputPerSymbol out
  
  subheader "Original Signals"
  Chart.input (inputSignals inp)

  subheader "Strategy"
  Chart.strategy (fmap impulseSignal ops) (alignedSignals out) (fmap outputSignal ops)
  
  let f sym outps acc = do
        acc
        
        subheader ("Symbol '" ++ show sym ++ "'")

        subsubheader ("Input Signal")
        toReport (SS.sampleStatistics (barLength inp) ((inputSignals inp) Map.! sym))
        
        subsubheader "Output Equity"
        toReport (SS.sampleStatistics (barLength inp) (outputSignal outps))

        subsubheader "Summary"
        toReport (summary outps)
  
        subsubheader "Trade Statistics"
        tradeStatistics (step inp) (sortedTrades outps)

        addendum sym

  Map.foldrWithKey' f (header "Analysis") ops
  
