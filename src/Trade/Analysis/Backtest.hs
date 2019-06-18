{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Backtest where

import Trade.Analysis.Report (BacktestData(..))

import Trade.Report.Basic (text)
import Trade.Report.ToReport (ToReport, toReport)


data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

noBacktest :: a -> b -> NoBacktestReport
noBacktest _ _ = NoBacktestReport

instance ToReport (BacktestData a NoBacktestReport) where
  toReport _ = text "No backtest done."
