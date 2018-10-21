{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Analysis.Optimize where

import qualified Trade.Type.Bars as B

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Type.ImpulseGenerator (ImpulseGenerator, OptimizedImpulseGenerator)


class Optimize optInput where
  type OptReportTy optInput :: *
  type TimeTy optInput :: *
  type OHLCTy optInput :: *
  optimize ::
    ImpulseGenerator optInput (TimeTy optInput) (OHLCTy optInput)
    -> optInput
    -> IO (OptimizedImpulseGenerator (TimeTy optInput) (OHLCTy optInput), OptReportTy optInput)


data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport

data NoTime
instance Eq NoTime
instance Ord NoTime
instance B.Time NoTime

data NoOHLC

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  type TimeTy NoOptimization = NoTime
  type OHLCTy NoOptimization = NoOHLC
  optimize strat NoOptimization = return (strat NoOptimization, NoOptimizationReport)

instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."
    
