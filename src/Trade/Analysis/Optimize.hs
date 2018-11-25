{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Analysis.Optimize where

import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), RankedStrategies(..))

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Analysis.OHLCData (OHLCData, OHLCDataTy, NoOHLC)

class Optimize optInput where
  type OptReportTy optInput :: *
  type OptInpTy optInput :: *

  optimize ::
    ImpulseGenerator (OptInpTy optInput) (OHLCDataTy optInput)
    -> optInput
    -> IO (RankedStrategies (OHLCDataTy optInput), OptReportTy optInput)


data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport

instance OHLCData NoOptimization where
  type OHLCDataTy NoOptimization = NoOHLC

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  type OptInpTy NoOptimization = NoOptimization
  
  optimize (ImpulseGenerator strat) NoOptimization =
    return (RankedStrategies [strat NoOptimization], NoOptimizationReport)

instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."

