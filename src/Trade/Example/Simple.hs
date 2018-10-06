{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Example.Simple where

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import qualified Trade.Type.Equity as Eqty
import qualified Trade.Type.OHLC as O

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.Signal.Impulse as IS
import qualified Trade.Type.Signal.Equity as ES

import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt

import qualified Trade.Report.Report as Report
import qualified Trade.Report.Curve as Curve

import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


ticker :: PS.PriceSignal OHLC.OHLC
ticker =
  let f x = OHLC.OHLC (O.Open (x+0.5)) (O.High (x+1)) (O.Low (x-1)) (O.Close x) (O.Volume 1000)
  in Signal.Signal (Vec.map (fmap f) TD.test2)
  
--------------------------------------------------------

data OptimizationInput = OptimizationInput (PS.PriceSignal OHLC.OHLC)

instance Opt.Optimize OptimizationInput where
  type OptTy OptimizationInput = OptimizationResult
  optimize strat (OptimizationInput _) = (strat, OptimizationResult)

data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData (OptimizationInput ps) OptimizationResult) =
    let toC (t, ohlc) =
          let c = E.Candle t
                (O.unOHLC $ OHLC.ohlcLow ohlc)
                (O.unOHLC $ OHLC.ohlcOpen ohlc)
                0
                (O.unOHLC $ OHLC.ohlcClose ohlc)
                (O.unOHLC $ OHLC.ohlcHigh ohlc)
          in c
        toCandle (Signal.Signal cs) = Vec.map toC cs

    in Report.subheader "Optimization Input"
       : Report.candle "Symbol" [toCandle ps]
       : Report.subheader "Optimization Result"
       : TR.toReport (TR.ReportString "No optimization has been done.")


--------------------------------------------------------

data BacktestInput = BacktestInput {
  tradeAt :: OHLC.OHLC -> O.Close
  , initialEquity :: Eqty.Equity
  , pricesInput :: PS.PriceSignal OHLC.OHLC
  }
    
instance BT.Backtest BacktestInput where
  type BackTy BacktestInput = BacktestResult
  type ImpGenTy BacktestInput = PS.PriceSignal OHLC.OHLC

  backtest optStrat (BacktestInput trdAt initEqty ps) =
    let impSig = optStrat ps
        es = BT.equitySignal trdAt initEqty impSig ps
    in BacktestResult impSig es

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal
  , eqties :: ES.EquitySignal
  }

instance TR.ToReport (TR.BacktestData BacktestInput BacktestResult) where
  toReport (TR.BacktestData (BacktestInput trdAt inEq ps) (BacktestResult impSig es)) =
    let bts = Vec.map (fmap Eqty.unEquity) (Signal.unSignal es)
        ps' = Vec.map (fmap (O.unOHLC . trdAt)) (Signal.unSignal ps)
        left = (Style.axTitle "Equity", [Report.lineL "Symbol at Close" ps', Report.lineL "Backtest" bts])
        right = (Style.impulseAxisConf, [Report.lineR "down buy / up sell" (Curve.curve impSig)])

    in Report.subheader "Backtest Result"
       : Report.svgLR (Style.axTitle "Time") left right
       : TR.toReport (TR.ReportString ("Initial Equity: " ++ show inEq))
       ++ TR.toReport (TR.ReportString ("Starting with equity " ++ show (Vec.head bts)))
       ++ TR.toReport (TR.ReportString ("Ending with equity " ++ show (Vec.last bts)))

--------------------------------------------------------

example :: IO ()
example = do
  

  let equity = Eqty.Equity 1
      trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optimalBuySell trdAt
        , Ana.optimizationInput = OptimizationInput ticker
        , Ana.backtestInput = BacktestInput trdAt equity ticker
        }

      rep = Ana.analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t

