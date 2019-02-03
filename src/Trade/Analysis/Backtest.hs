{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Analysis.Backtest where

import Trade.Type.Bars (Add)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)

import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal (DeltaSignal)
import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)

import Trade.Type.Equity (Equity(..))

import Trade.Type.ImpulseGenerator (NonEmptyList, OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Signal (Signal(..))
import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Trade (Trade(..), TradeList(..))


import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)



data Experiment stgy t ohlc = Experiment {
  step :: StepTy stgy t
  , equity :: Equity
  , impulseSignal :: ImpulseSignal t
  , signal :: Signal t ohlc
  }




equitySignal ::
  forall ohlc t stgy.
  ( Show t, Show ohlc
  , ToDelta ohlc, Ord t, Add t
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , StepFunction (StepTy stgy) t) =>
  Experiment stgy t ohlc -> Signal t Equity
equitySignal (Experiment stp eqty impSig ps) =
  let ts :: TradeList stgy t ohlc
      ts = impulse2tradeList ps impSig
      dts = tradeList2DeltaTradeList ts
  in concatDeltaSignals stp eqty dts


class Backtest btInput where
  type BacktestReportTy btInput :: *
  
  backtest :: NonEmptyList (OptimizedImpulseGenerator (OHLCDataTy btInput)) -> btInput -> BacktestReportTy btInput


data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

instance OHLCData NoBacktest where
  type OHLCDataTy NoBacktest = NoOHLC

instance Backtest NoBacktest where
  type BacktestReportTy NoBacktest = NoBacktestReport
  backtest _ NoBacktest = NoBacktestReport

instance ToReport (BacktestData NoBacktest NoBacktestReport) where
  toReport _ = Rep.text "No backtest done."
