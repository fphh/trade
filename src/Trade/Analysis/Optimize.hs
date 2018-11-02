{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Analysis.Optimize where

import Data.Time.Clock (UTCTime)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Type.ImpulseGenerator (ImpulseGenerator, OptimizedImpulseGenerator)


class Optimize optInput where
  type OptReportTy optInput :: *
  type OHLCTy optInput :: *
  optimize ::
    ImpulseGenerator optInput UTCTime (OHLCTy optInput)
    -> optInput
    -> IO (OptimizedImpulseGenerator UTCTime (OHLCTy optInput), OptReportTy optInput)


data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport

data NoOHLC

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  type OHLCTy NoOptimization = NoOHLC
  optimize strat NoOptimization = return (strat NoOptimization, NoOptimizationReport)

instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."
    
