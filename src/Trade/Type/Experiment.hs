{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.Bars (Add)
import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)
import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.Equity (Equity)
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal, curve)
import Trade.Type.Signal (Signal(..))
import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Trade (TradeList)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)
import Trade.Type.Conversion.Type2Double (Type2Double)

import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style

import Trade.Help.SafeTail (shead, slast)

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
  ( Show t, Show ohlc
  , ToDelta ohlc, Ord t, Add t
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


render ::
  (Show t, Ord t, PlotValue t, Type2Double ohlc) =>
  String -> String -> Result stgy t ohlc -> Rep.HtmlIO
render symTitle btTitle (Result inp out) = do
  let hd = show . shead "Experiment.render (hd)" . unSignal
      lst = show . slast "Experiment.render (lst)" . unSignal

  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity")
      [ Line.line symTitle (inputSignal inp)
      , Line.line btTitle (outputSignal out)])
    (Rep.impulseSignalCharts [curve (inputSignal inp) (impulseSignal out)])

  Rep.text ("Initial Equity: " ++ show (initialEquity inp))
  Rep.text ("Starting with equity " ++ hd (outputSignal out))
  Rep.text ("Ending with equity " ++ lst (outputSignal out))
