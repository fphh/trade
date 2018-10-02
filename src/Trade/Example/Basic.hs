{-# LANGUAGE TypeFamilies #-}


module Trade.Example.Basic where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt

import qualified Trade.Report.Report as Report

--------------------------------------------------------

data OptimizationResult = OptimizationResult

instance TR.ToReport OptimizationResult where
  toReport OptimizationResult = TR.toReport (TR.ReportString "Nothing to optimize.")
  
data OptimizationInput = OptimizationInput

instance Opt.Optimize OptimizationInput where
  type OptTy OptimizationInput = OptimizationResult
  optimize strat OptimizationInput = (strat, OptimizationResult)

--------------------------------------------------------

data BacktestInput = BacktestInput

data BacktestResult = BacktestResult

instance TR.ToReport BacktestResult where
  toReport BacktestResult = TR.toReport (TR.ReportString "Nothing to report.")

instance BT.Backtest BacktestInput where
  type BackTy BacktestInput = BacktestResult
  type ImpGenTy BacktestInput = PS.PriceSignal ()

  backtest impGen bt = BacktestResult

--------------------------------------------------------

example :: IO ()
example = do
  

  let analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.impulseGenerator = IG.noImpulses
        , Ana.optimizationInput = OptimizationInput
        , Ana.backtestInput = BacktestInput
        }

      rep = Ana.analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t

