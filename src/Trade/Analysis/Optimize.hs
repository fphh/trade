{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Analysis.Optimize where

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Type.ImpulseGenerator (ImpulseGenerator, OptimizedImpulseGenerator)


class Optimize optInput where
  type OptReportTy optInput :: * -> *
  type TimeTy optInput :: * 
  optimize ::
 --   (Ord t, B.Time t) =>
    ImpulseGenerator (TimeTy optInput) optInput ohlc
    -> optInput (TimeTy optInput) ohlc
    -> IO (OptimizedImpulseGenerator (TimeTy optInput) ohlc, OptReportTy optInput) -- (TimeTy optInput))



data NoOptimization t ohlc = NoOptimization

data NoOptimizationReport t = NoOptimizationReport

data NoTime

instance Optimize NoOptimization where
  type OptReportTy NoOptimization = NoOptimizationReport
  type TimeTy NoOptimization = NoTime
  optimize strat NoOptimization = return (strat NoOptimization, NoOptimizationReport)

instance ToReport (OptimizationData t ohlc NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."
    
