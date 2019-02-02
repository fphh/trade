{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Analysis.Backtest where

-- import qualified Test.QuickCheck as QC

import Trade.Type.Bars (Add)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)

import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal (DeltaSignal)
import Trade.Type.DeltaSignal.Algorithm (toDeltaSignal, shortDeltaSignal, constDeltaSignal, concatDeltaSignals)

import Trade.Type.Equity (Equity(..))

import Trade.Type.ImpulseGenerator (NonEmptyList, OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Position (Position(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Strategy (Long, Short) -- Strategy(..))
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


class Delta2DeltaTrade stgy where
  trade2deltaTrade :: (ToDelta ohlc, Add t) => TradeList stgy t ohlc -> [DeltaSignal t ohlc]

instance Delta2DeltaTrade Long where
  trade2deltaTrade (TradeList tl) =
    let f (Trade NotInvested ts) = constDeltaSignal (toDeltaSignal (Signal ts))
        f (Trade Invested ts) = toDeltaSignal (Signal ts)
    in map f tl

    
instance Delta2DeltaTrade Short where
  trade2deltaTrade (TradeList tl) =
    let f (Trade NotInvested ts) = constDeltaSignal (toDeltaSignal (Signal ts))
        f (Trade Invested ts) = shortDeltaSignal (toDeltaSignal (Signal ts))
    in map f tl



equitySignal ::
  forall ohlc t stgy.
  ( Show t, Show ohlc
  , ToDelta ohlc, Ord t, Add t
  , Delta2DeltaTrade stgy
  , Impulse2TradeList stgy
  , StepFunction (StepTy stgy) t) =>
  Experiment stgy t ohlc -> Signal t Equity
equitySignal (Experiment stp eqty impSig ps) =
  let ts :: TradeList stgy t ohlc
      ts = impulse2tradeList ps impSig
      dts = trade2deltaTrade ts
      res = concatDeltaSignals stp eqty dts
  in res


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
