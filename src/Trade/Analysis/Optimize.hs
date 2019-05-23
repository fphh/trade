{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Analysis.Optimize where

import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), RankedStrategies(..))

import Trade.Analysis.Report(OptimizationData(..))
import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

import Trade.Report.Basic (text)
import Trade.Report.ToReport (ToReport, toReport)

class Optimize optInput where
  type OptReportTy optInput :: *
  type OptInpTy optInput :: *

  optimize ::
    ImpulseGenerator (OptInpTy optInput) (OHLCDataTy optInput)
    -> optInput
    -> (RankedStrategies (OHLCDataTy optInput), OptReportTy optInput)


data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport

instance OHLCData NoOptimization where
  type OHLCDataTy NoOptimization = NoOHLC

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  type OptInpTy NoOptimization = NoOptimization

  optimize (ImpulseGenerator strat) NoOptimization =
    (RankedStrategies [strat NoOptimization], NoOptimizationReport)

instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = text "No optimization was done."

