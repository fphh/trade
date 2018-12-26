{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Backtest where


import qualified Data.Vector as Vec


-- import Trade.Type.Bars (Time, DeltaT, diff)
-- import Trade.Type.Delta (Delta, DeltaSignal, AddDelta, AddDeltaTy, Scale, toDeltaSignal, constDeltaSignal, shortDeltaSignal)


import Trade.Type.Delta (Delta(..), DeltaTy, DDelta, CDelta, Add, add, diff)
import Trade.Type.Scale (Scale, scale, factor)

import Trade.Type.DeltaSignal (DeltaSignal, toDeltaSignal, shortDeltaSignal, constDeltaSignal)

import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (NonEmptyList, OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Position (Position(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Strategy (Strategy(..))
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.Yield (Yield(..))
import Trade.Type.Conversion.Yield2Equity (Yield2Equity, yield2equity)
-- import Trade.Type.Conversion.Trade2TradeYield (trade2tradeYield)
import Trade.Type.Conversion.Impulse2TradeList (impulse2tradeList)
-- import Trade.Type.Conversion.TradeYield2YieldSignal (tradeYield2yieldSignal)
import Trade.Type.Conversion.Type2Double (Type2Double)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

import Trade.Help.SafeTail (shead)

import Debug.Trace


data Experiment yield t ohlc a = Experiment {
  strategy :: Strategy
  , tradeAt :: ohlc -> a
  , stepFunc :: StepFunc yield
  , equity :: Equity
  , impulseSignal :: ImpulseSignal t
  , signal :: Signal t ohlc
  }

{-
mirrorShortPositions :: (Num a) => TradeList t a -> TradeList t a
mirrorShortPositions (TradeList tl) =
  let g dx0 (t, x) = (t, dx0 - x + dx0)
      f (Trade ShortPosition ts) =
        let h = snd (shead "mirrorShortPositions" ts)
        in Trade ShortPosition (Vec.map (g h) ts)
      f trd = trd
  in TradeList (map f tl)
-}


trade2deltaTrade ::
  (Add t, Add a, Scale a, Scale (DeltaTy a), DeltaTy a ~ Delta Double a) =>
  TradeList t a -> [DeltaSignal t a]
trade2deltaTrade (TradeList tl) =
  let f (Trade NoPosition ts) = constDeltaSignal (toDeltaSignal (Signal ts))
      f (Trade LongPosition ts) = toDeltaSignal (Signal ts)
      f (Trade ShortPosition ts) = shortDeltaSignal (toDeltaSignal (Signal ts))
  in map f tl

{-
equitySignal ::
  (Ord t, Add t, Add ohlc, Scale ohlc, Show t
  , Show ohlc, DeltaTy ohlc ~ CDelta ohlc
  , DeltaTy t ~ DDelta t) =>
  Experiment Yield t a ohlc -> EquitySignal t
  -}
-- equitySignal :: _
equitySignal (Experiment stgy tradeAt stepFunc eqty impSig qs@(Signal ps)) =
  let (start, dt) =
        case (ps Vec.!? 0, ps Vec.!? 1) of
          (Just (t0, _), Just (t1, _)) -> (t0, t1 `diff` t0)
          _ -> error "Trade.Analysis.Backtest.equitySignal: price signal to short"

      ts = impulse2tradeList stgy qs impSig
      dts = trade2deltaTrade (fmap tradeAt ts)
      
      -- nts = trade2tradeYield ss
      -- res = tradeYield2yieldSignal stepFunc eqty start dt nts

  in trace (show dts) undefined


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
  

