{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Backtest where


import qualified Data.Vector as Vec


import Trade.Type.Bars (Time, DeltaT, diff)
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Yield (Yield)
import Trade.Type.OHLC (UnOHLC)
import Trade.Type.NormTrade (NormTradeList(..), NormTrade(..))
import Trade.Type.Trade (TradeList(..), Trade(..))
import Trade.Type.Conversion.Yield2Equity (yield2equity)
import Trade.Type.Conversion.Trade2NormTrade (trade2normTrade)
import Trade.Type.Conversion.Impulse2Trade (impulse2trade)
import Trade.Type.Conversion.NormTrade2YieldSignal (normTrade2yieldSignal)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

equitySignal ::
  (UnOHLC a, Ord t, Time t, Num (DeltaT t)) =>
  (ohlc -> a) ->
  StepFunc Yield -> Equity -> ImpulseSignal t -> Signal t ohlc -> EquitySignal t
equitySignal tradeAt stepFunc eqty impSig qs@(Signal ps) =
  let (start, dt) =
        case (ps Vec.!? 0, ps Vec.!? 1) of
          (Just (t0, _), Just (t1, _)) -> (t0, t1 `diff` t0)
          _ -> error "Trade.Analysis.Backtest.equitySignal: price signal to short"

      ts = impulse2trade qs impSig
      nts = trade2normTrade (fmap tradeAt ts)
      res = yield2equity stepFunc eqty (normTrade2yieldSignal start dt nts)

  in res

class Backtest btInput where
  type BacktestReportTy btInput :: *
  
  backtest :: OptimizedImpulseGenerator (OHLCDataTy btInput) -> btInput -> BacktestReportTy btInput


data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

instance OHLCData NoBacktest where
  type OHLCDataTy NoBacktest = NoOHLC

instance Backtest NoBacktest where
  type BacktestReportTy NoBacktest = NoBacktestReport
  backtest _ NoBacktest = NoBacktestReport

instance ToReport (BacktestData NoBacktest NoBacktestReport) where
  toReport _ = Rep.text "No backtest done."
  

    
