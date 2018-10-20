{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Optimize where


import Data.Time.Clock (UTCTime)

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Type.ImpulseGenerator (ImpulseGenerator, OptimizedImpulseGenerator)


class Optimize optInput where
  type OptReportTy optInput :: *
  optimize :: ImpulseGenerator t optInput ohlc -> optInput ohlc -> IO (OptimizedImpulseGenerator t ohlc, OptReportTy optInput)



data NoOptimization ohlc = NoOptimization

data NoOptimizationReport = NoOptimizationReport

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  optimize strat NoOptimization = return (strat NoOptimization, NoOptimizationReport)

instance ToReport (OptimizationData ohlc NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."
    
