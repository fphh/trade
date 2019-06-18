{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Optimize where

import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), RankedStrategies(..))

import Trade.Analysis.Report(OptimizationData(..))

import Trade.Report.Basic (text)
import Trade.Report.ToReport (ToReport, toReport)


data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport

optimize ::
  ImpulseGenerator NoOptimization ohlc
  -> NoOptimization
  -> (RankedStrategies ohlc, NoOptimizationReport)
optimize (ImpulseGenerator strat) NoOptimization =
  (RankedStrategies [strat NoOptimization], NoOptimizationReport)


emptyOptimize :: ImpulseGenerator optInpTy ohlc -> optInp -> (RankedStrategies ohlc, NoOptimizationReport)
emptyOptimize _ _ = (RankedStrategies [], NoOptimizationReport)


instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = text "No optimization was done."
