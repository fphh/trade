{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

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
  type BacktestReportTy btInput :: *
  
  backtest :: OptimizedImpulseGenerator UTCTime ohlc -> btInput ohlc -> BacktestReportTy btInput

data NoBacktest ohlc = NoBacktest

data NoBacktestReport = NoBacktestReport

instance Backtest NoBacktest where
  type BacktestReportTy NoBacktest = NoBacktestReport
  backtest _ NoBacktest = NoBacktestReport

instance ToReport (BacktestData ohlc NoBacktest NoBacktestReport) where
  toReport _ = Rep.text "No backtest done."
  

    
