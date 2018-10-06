{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Optimize where

import qualified Trade.Report.Report as Rep
import Trade.Analysis.ToReport (ToReport, toReport, OptimizationData(..))

import Trade.Type.ImpulseGenerator (ImpulseGenerator)


class Optimize a where
  type OptTy a :: *
  optimize :: ImpulseGenerator impGenInp -> a -> (ImpulseGenerator impGenInp, OptTy a)



data NoOptimization = NoOptimization

data NoOptimizationReport = NoOptimizationReport


instance Optimize NoOptimization where
  type OptTy NoOptimization = NoOptimizationReport
  optimize strat NoOptimization = (strat, NoOptimizationReport)

instance ToReport (OptimizationData NoOptimization NoOptimizationReport) where
  toReport _ = Rep.text "No optimization was done."
    
