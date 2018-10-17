{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Example.Basic where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt

import qualified Trade.Report.Report as Rep

--------------------------------------------------------
  
data OptimizationInput ohlc = OptimizationInput

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  optimize strat OptimizationInput = return (strat OptimizationInput, OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData ohlc OptimizationInput OptimizationResult) where
  toReport _ = do
    Rep.subheader "Optimization"
    Rep.text "Nothing to optimize."
    
--------------------------------------------------------

data BacktestInput ohlc = BacktestInput

instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult
  backtest _ _ = BacktestResult


data BacktestResult = BacktestResult

instance TR.ToReport (TR.BacktestData ohlc BacktestInput BacktestResult) where
  toReport _ = do
    Rep.subheader "Backtest"
    Rep.text "Nothing to report."
    
--------------------------------------------------------

example :: IO ()
example = do
  

  let analysis :: Ana.Analysis () OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "Basic Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen IG.noImpulses
        , Ana.optimizationInput = OptimizationInput
        , Ana.backtestInput = BacktestInput
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t

