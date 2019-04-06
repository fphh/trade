{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where



import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Text.Printf (printf)


import Trade.Type.Bars (DeltaTy, {- FormatDelta, -} Add, diff {- , formatDelta -} )
import Trade.Type.Delta (Delta(..), ToDelta)
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)

import Trade.Type.DeltaSignal (DeltaSignal)
import qualified Trade.Type.DeltaSignal.Algorithm as DSA

import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal, curve)

import qualified Trade.Type.NestedMap as NestedMap
import Trade.Type.NestedMap (NestedMap)

import Trade.Type.Price (Price)

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Trade (TradeList)
import Trade.Type.Yield (LogYield, ToYield, toYield, logYield2yield)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.SparkLine as Spark
import qualified Trade.Report.Style as Style
import qualified Trade.Report.Table as Table

import qualified Trade.TStatistics.SampleStatistics as SS
import qualified Trade.TStatistics.Statistics as Stats
import qualified Trade.TStatistics.TradeStatistics as TS
import qualified Trade.TStatistics.YieldStatistics as YS


import Trade.Help.SafeTail (shead, slast)

import Trade.Report.HtmlIO (liftHtml, toHtmlIO, HtmlIO)
import Trade.Report.Pretty (pretty, Pretty)


data Input stgy t ohlc = Input {
  step :: StepTy stgy t
  , initialEquity :: Equity
  , impulseGenerator :: OptimizedImpulseGenerator ohlc
  , inputSignal :: Signal t ohlc
  }

data Output t ohlc = Output {
  impulseSignal :: ImpulseSignal t
  , deltaTradeList :: DeltaTradeList t ohlc
  , outputSignal :: Signal t Equity
  }


data Result stgy t ohlc = Result {
  input :: Input stgy t ohlc
  , output :: Output t ohlc
  }

conduct ::
  forall ohlc t stgy.
  (ToDelta ohlc, Ord t, Add t
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , StepFunction (StepTy stgy) t) =>
  Input stgy t ohlc -> Result stgy t ohlc
conduct inp@(Input stp eqty (OptimizedImpulseGenerator impGen) ps) =
  let ts :: TradeList stgy t ohlc
      impSig = impGen ps
      ts = impulse2tradeList ps impSig
      dts = tradeList2DeltaTradeList ts

      out = Output {
        impulseSignal = impSig
        , deltaTradeList = dts
        , outputSignal = concatDeltaSignals stp eqty dts
        }
      
  in Result {
    input = inp
    , output = out
    }


tradeStatistics ::
  (StepFunction (StepTy stgy) t, Eq t, Add t
  , Real (DeltaTy t)
  , Pretty (DeltaTy t), Pretty (Stats.DeltaTyStats t)) =>
  StepTy stgy t -> DeltaTradeList t ohlc -> HtmlIO
tradeStatistics step dtl =

  let ts = DSA.sortDeltaSignals dtl
      sparks = Spark.toSparkLine step ts
      stats = YS.yieldStatistics ts -- (TS.yieldStatistics ts, TS.tradeStatistics ts)

      f _ _ st sp = toHtmlIO st <> toHtmlIO sp
      zs = NestedMap.zipWith f stats sparks

      g pos wl htmlio =
        let sty = H5A.style (H5.stringValue "clear:both;margin:18px;padding-top:24px;color:#006600")
            header = (H5.div ! sty) (H5.b (H5.preEscapedToHtml (show pos ++ "/" ++ show wl)))
        in liftHtml (header <>) htmlio
        
  in NestedMap.fold g zs
  

lastEquity :: Result stgy t ohlc -> Equity
lastEquity (Result _ out) = snd (slast "Experiment.lastEquity" (unSignal (outputSignal out)))

render ::
  (Ord t, PlotValue t
  , StepFunction (StepTy stgy) t
  , Pretty t, Pretty (DeltaTy t), Pretty (Stats.DeltaTyStats t), Pretty ohlc
  , ToYield ohlc
  , Add t, Ord (Delta ohlc)
  , Num (DeltaTy t)
  , Real (DeltaTy t)
  , Line.TyX (Signal t ohlc) ~ t, Line.TyY (Signal t ohlc) ~ Double, Line.Line (Signal t ohlc)) =>
  String -> String -> Result stgy t ohlc -> HtmlIO
render symTitle btTitle (Result inp out) = do
  
  Rep.subheader "Experiment"
  
  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity")
      [ Line.line symTitle (inputSignal inp)
      , Line.line btTitle (outputSignal out)])
    (Rep.impulseSignalCharts [curve (inputSignal inp) (impulseSignal out)])

  Rep.subsubheader "Summary"

  toHtmlIO (SS.sampleStatistics (inputSignal inp))
  toHtmlIO (SS.sampleStatistics (outputSignal out))

  Rep.subsubheader "Trade statistics"

  tradeStatistics (step inp) (deltaTradeList out)
