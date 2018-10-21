{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Analysis.Backtest where

import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC)
import Trade.Type.Conversion.Trade2Equity (trade2equity)
import Trade.Type.Conversion.Impulse2Trade (impulse2trade)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, BacktestData(..))

equitySignal ::
  (UnOHLC a, Ord t) =>
  (ohlc -> a) -> Equity -> ImpulseSignal t -> Signal t ohlc -> EquitySignal t
equitySignal tradeAt eqty impSig qs = 
  let ts = impulse2trade qs impSig
  in trade2equity tradeAt eqty ts
  
class Backtest btInput where
  type BacktestReportTy btInput :: * -> *
  
  backtest ::
    (Ord t) => OptimizedImpulseGenerator t ohlc -> btInput t ohlc -> (BacktestReportTy btInput) t

data NoBacktest t ohlc = NoBacktest

data NoBacktestReport t = NoBacktestReport

instance Backtest NoBacktest where
  type BacktestReportTy NoBacktest = NoBacktestReport
  backtest _ NoBacktest = NoBacktestReport

instance ToReport (BacktestData t ohlc NoBacktest NoBacktestReport) where
  toReport _ = Rep.text "No backtest done."
  

    
