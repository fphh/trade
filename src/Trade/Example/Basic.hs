{-# LANGUAGE TypeFamilies #-}


module Trade.Example.Example0 where

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

data BacktestInput symbol ohlc = BacktestInput {
  symbol :: symbol
  , outOfSample :: PS.PriceSignal ohlc
  }

data BacktestResult symbol ohlc = BacktestResult

instance TR.ToReport (BacktestResult symbol ohlc) where
  toReport BacktestResult = TR.toReport (TR.ReportString "Nothing to report.")

instance BT.Backtest (BacktestInput symbol ohlc) where
  type BackTy (BacktestInput symbol ohlc) = BacktestResult symbol ohlc
  type ImpGenTy (BacktestInput symbol ohlc) = PS.PriceSignal ohlc

  backtest impGen bt = BacktestResult

--------------------------------------------------------

data Symbol dbCode = Symbol

--------------------------------------------------------

example0 :: IO ()
example0 = do
  

  let backtest = BacktestInput {
        symbol = Symbol
        , outOfSample = Signal.noSignal
        }

      analysis :: Ana.Analysis OptimizationInput (BacktestInput (Symbol dbCode) ())
      analysis = Ana.Analysis {
        Ana.impulseGenerator = IG.noImpulses
        , Ana.optimizationInput = OptimizationInput
        , Ana.backtestInput = backtest
        }

      rep = Ana.analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t

