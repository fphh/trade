{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Backtest where


import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator)
import Trade.Type.NonEmptyList (NonEmptyList)

import Trade.Analysis.Report (BacktestData(..))
import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

import Trade.Report.Basic (text)
import Trade.Report.ToReport (ToReport, toReport)


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
  toReport _ = text "No backtest done."
