{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Backtest where


import qualified Data.Vector as Vec


import Trade.Type.Bars (Time, DeltaT, diff)
import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseGenerator (NonEmptyList, OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Strategy (Strategy(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Yield (ToYield, NoYield, Yield, LogYield)
import Trade.Type.Conversion.Yield2Equity (Yield2Equity, yield2equity)
import Trade.Type.Conversion.Trade2NormTrade (trade2normTrade)
import Trade.Type.Conversion.Impulse2TradeList (impulse2tradeList)
import Trade.Type.Conversion.NormTrade2YieldSignal (normTrade2yieldSignal)
import Trade.Type.Conversion.Type2Double (Type2Double)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

data Experiment yield t ohlc a = Experiment {
  strategy :: Strategy
  , tradeAt :: ohlc -> a
  , stepFunc :: StepFunc yield
  , equity :: Equity
  , impulseSignal :: ImpulseSignal t
  , signal :: Signal t ohlc
  }



equitySignal ::
  (Type2Double a, Ord t, Time t, Num (DeltaT t), NoYield yield, ToYield yield, Yield2Equity yield) =>
  Experiment yield t ohlc a -> EquitySignal t
equitySignal (Experiment Long tradeAt stepFunc eqty impSig qs@(Signal ps)) =
  let (start, dt) =
        case (ps Vec.!? 0, ps Vec.!? 1) of
          (Just (t0, _), Just (t1, _)) -> (t0, t1 `diff` t0)
          _ -> error "Trade.Analysis.Backtest.equitySignal: price signal to short"

      ts = impulse2tradeList qs impSig
      nts = trade2normTrade (fmap tradeAt ts)
      res = yield2equity stepFunc eqty (normTrade2yieldSignal start dt nts)

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
  

    
