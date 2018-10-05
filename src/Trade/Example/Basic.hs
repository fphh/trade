{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Example.Basic where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt

import qualified Trade.Report.Report as Report

--------------------------------------------------------
  
data OptimizationInput = OptimizationInput

instance Opt.Optimize OptimizationInput where
  type OptTy OptimizationInput = OptimizationResult
  optimize strat OptimizationInput = (strat, OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OptimizationInput OptimizationResult) where
  toReport _ =
    Report.subheader "Optimization"
    : TR.toReport (TR.ReportString "Nothing to optimize.")
    
--------------------------------------------------------

data BacktestInput = BacktestInput

instance BT.Backtest BacktestInput where
  type BackTy BacktestInput = BacktestResult
  type ImpGenTy BacktestInput = PS.PriceSignal ()

  backtest impGen bt = BacktestResult


data BacktestResult = BacktestResult

instance TR.ToReport (TR.BacktestData BacktestInput BacktestResult) where
  toReport _ =
    Report.subheader "Backtest"
    : TR.toReport (TR.ReportString "Nothing to report.")
    
--------------------------------------------------------

example :: IO ()
example = do
  

  let analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "Basic Report"
        , Ana.impulseGenerator = IG.noImpulses
        , Ana.optimizationInput = OptimizationInput
        , Ana.backtestInput = BacktestInput
        }

      rep = Ana.analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t

