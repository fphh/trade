{-# LANGUAGE TypeFamilies #-}

module Trade.Analysis.Optimize where

import qualified Trade.Report.Report as Report
import Trade.Analysis.ToReport (ToReport, toReport)

import Trade.Type.ImpulseGenerator (ImpulseGenerator)


class Optimize a where
  type OptTy a :: *
  optimize :: ImpulseGenerator impGenInp -> a -> (ImpulseGenerator impGenInp, OptTy a)



data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport


instance Optimize NoOptimization where
  type OptTy NoOptimization = NoOptimizationReport
  optimize strat NoOptimization = (strat, NoOptimizationReport)

instance ToReport NoOptimizationReport where
  toReport NoOptimizationReport =
    [ Report.text "No optimizations done." ]
               

    
