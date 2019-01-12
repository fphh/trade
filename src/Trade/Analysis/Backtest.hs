{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec

import Trade.Type.Bars (DeltaTy, Add)

import Trade.Type.Conversion.Impulse2TradeList (impulse2tradeList)

import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal (DeltaSignal)
import Trade.Type.DeltaSignal.Algorithm (toDeltaSignal, shortDeltaSignal, constDeltaSignal, concatDeltaSignals)

import Trade.Type.Equity (Equity(..))

import Trade.Type.ImpulseGenerator (NonEmptyList, OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Position (Position(..))
import Trade.Type.Price (Price)
import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.Strategy (Strategy(..))
import Trade.Type.Trade (Trade(..), TradeList(..))


import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

import Trade.Help.SafeTail (shead)

import Debug.Trace


data Experiment t ohlc = Experiment {
  strategy :: Strategy
  , equity :: Equity
  , impulseSignal :: ImpulseSignal t
  , signal :: Signal t ohlc
  }

trade2deltaTrade :: (ToDelta ohlc, Add t) => TradeList t ohlc -> [DeltaSignal t ohlc]
trade2deltaTrade (TradeList tl) =
  let f (Trade NoPosition ts) = constDeltaSignal (toDeltaSignal (Signal ts))
      f (Trade LongPosition ts) = toDeltaSignal (Signal ts)
      f (Trade ShortPosition ts) = shortDeltaSignal (toDeltaSignal (Signal ts))
  in map f tl


equitySignal ::
  (ToDelta ohlc, Ord t, Add t, Real (DeltaTy t)) =>
  Experiment t ohlc -> Signal t Equity
equitySignal (Experiment stgy eqty impSig qs@(Signal ps)) =
  let ts = impulse2tradeList stgy qs impSig
      dts = trade2deltaTrade ts
  in concatDeltaSignals eqty dts


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
