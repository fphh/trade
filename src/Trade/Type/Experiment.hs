{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Text.Printf (printf)


import Trade.Type.Bars (Add)
import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)
import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal, curve)
import Trade.Type.Price (Price)
import Trade.Type.Signal (Signal(..))
import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Trade (TradeList)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import qualified Trade.Report.Table as Table

import Trade.Help.SafeTail (shead, slast)

import Trade.Report.HtmlIO (HtmlIO)


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


lastEquity :: Result stgy t ohlc -> Equity
lastEquity (Result _ out) = snd (slast "Experiment.lastEquity" (unSignal (outputSignal out)))

render ::
  ( Show t, Ord t, PlotValue t
  , Line.TyX (Signal t ohlc) ~ t, Line.TyY (Signal t ohlc) ~ Double, Line.Line (Signal t ohlc)) =>
  String -> String -> Result stgy t ohlc -> HtmlIO
render symTitle btTitle (Result inp out) = do
  let format = printf "%.2f"
      f (x, y) = [show x, format (unEquity y)]
      hd = f . shead "Experiment.render (hd, 1)" . unSignal
      lst = f . slast "Experiment.render (lst, 2)" . unSignal

      Equity initial = initialEquity inp
      (_, Equity final) = slast "Experiment.render (lst, 3)" (unSignal (outputSignal out))

      (_, input1) = shead "Experiment.render (hd, 4)" (unSignal (inputSignal inp))
      (_, inputN) = slast "Experiment.render (lst, 5)" (unSignal (inputSignal inp))

  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity")
      [ Line.line symTitle (inputSignal inp)
      , Line.line btTitle (outputSignal out)])
    (Rep.impulseSignalCharts [curve (inputSignal inp) (impulseSignal out)])

  Table.textTable [
    [ "Initial equity", "", format (unEquity (initialEquity inp)) ]
    , []
    , "Starting with equity" : hd (outputSignal out)
    , "Ending with equity" : lst (outputSignal out)
    , []
    , [ "Ratio final / initial equity", "", show (final / initial) ]
    ]
