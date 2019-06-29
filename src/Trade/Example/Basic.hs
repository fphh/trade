{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Example.Basic where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Trade.Type.ImpulseGenerator as IG

import Trade.Analysis.Analysis (Analysis(..), analyzeHelper)
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Report as ARep

import Trade.Report.Basic (subheader, text)
import Trade.Report.Config (HtmlReader)
import Trade.Report.HtmlReader (render)
import qualified Trade.Report.ToReport as TR

--------------------------------------------------------
  
data OptimizationInput = OptimizationInput

optimize ::
  IG.ImpulseGenerator OD.NoOHLC ohlc
  -> OptimizationInput
  -> (IG.RankedStrategies ohlc, OptimizationResult)
optimize (IG.ImpulseGenerator strat) OptimizationInput =
  (IG.RankedStrategies [strat OD.NoOHLC], OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (ARep.OptimizationData OptimizationInput OptimizationResult) where
  toReport _ = do
    subheader "Optimization"
    text "Nothing to optimize."
    
--------------------------------------------------------

data BacktestInput = BacktestInput

backtest :: a -> b -> BacktestResult
backtest _ _ = BacktestResult


data BacktestResult = BacktestResult

instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  toReport _ = do
    subheader "Backtest"
    text "Nothing to report."
    
--------------------------------------------------------

analyze :: Analysis OptimizationInput BacktestInput OD.NoOHLC ohlc -> HtmlReader ()
analyze = analyzeHelper optimize backtest

--------------------------------------------------------


example :: IO ()
example = do

  let analysis :: Analysis OptimizationInput BacktestInput _a _b
      analysis = Analysis {
        title = "Basic Report"
        , impulseGenerator = IG.noImpulses
        , optimizationInput = OptimizationInput
        , backtestInput = BacktestInput
        }

      rep = analyze analysis

  t <- render rep
  
  BSL.putStrLn t

