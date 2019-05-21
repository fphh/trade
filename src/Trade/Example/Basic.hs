{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Example.Basic where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Report as ARep

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.ToReport as TR

--------------------------------------------------------
  
data OptimizationInput = OptimizationInput

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  type OptInpTy OptimizationInput = OD.NoOHLC

  optimize (IG.ImpulseGenerator strat) OptimizationInput =
    (IG.RankedStrategies [strat OD.NoOHLC], OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (ARep.OptimizationData OptimizationInput OptimizationResult) where
  toReport _ = do
    Rep.subheader "Optimization"
    Rep.text "Nothing to optimize."
    
--------------------------------------------------------

data BacktestInput = BacktestInput

instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult
  backtest _ _ = BacktestResult


data BacktestResult = BacktestResult

instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  toReport _ = do
    Rep.subheader "Backtest"
    Rep.text "Nothing to report."
    
--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = OD.NoOHLC

instance OD.OHLCData BacktestInput where
  type OHLCDataTy BacktestInput = OD.NoOHLC

--------------------------------------------------------


example :: IO ()
example = do

  let analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "Basic Report"
        , Ana.impulseGenerator = undefined -- IG.optImpGen2impGen IG.noImpulses
        , Ana.optimizationInput = OptimizationInput
        , Ana.backtestInput = BacktestInput
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t

