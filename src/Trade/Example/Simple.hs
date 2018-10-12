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

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Curve as Curve

import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


ticker :: PS.PriceSignal OHLC.OHLC
ticker =
  let f x = OHLC.OHLC (O.Open (x+0.5)) (O.High (x+1)) (O.Low (x-1)) (O.Close x) (O.Volume 1000)
  in Signal.Signal (Vec.map (fmap f) TD.test2)
  
--------------------------------------------------------

data OptimizationInput ohlc = OptimizationInput (PS.PriceSignal ohlc)

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  optimize strat optInput = return (strat optInput, OptimizationResult)

data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OHLC.OHLC OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData (OptimizationInput ps) OptimizationResult) = do
    let toC (t, ohlc) =
          let c = E.Candle t
                (O.unOHLC $ OHLC.ohlcLow ohlc)
                (O.unOHLC $ OHLC.ohlcOpen ohlc)
                0
                (O.unOHLC $ OHLC.ohlcClose ohlc)
                (O.unOHLC $ OHLC.ohlcHigh ohlc)
          in c
        toCandle (Signal.Signal cs) = Vec.map toC cs


    
    Rep.subheader "Optimization Input"
    Rep.candle "Symbol" [toCandle ps]
    Rep.subheader "Optimization Result"
    Rep.text "No optimization has been done."


--------------------------------------------------------

data BacktestInput ohlc = BacktestInput {
  tradeAt :: ohlc -> O.Close
  , initialEquity :: Eqty.Equity
  , pricesInput :: PS.PriceSignal ohlc
  }
    
instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult

  backtest optStrat (BacktestInput trdAt initEqty ps) =
    let impSig = optStrat ps
        es = BT.equitySignal trdAt initEqty impSig ps
    in BacktestResult impSig es

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal
  , eqties :: ES.EquitySignal
  }

instance TR.ToReport (TR.BacktestData OHLC.OHLC BacktestInput BacktestResult) where
  toReport (TR.BacktestData (BacktestInput trdAt inEq ps) (BacktestResult impSig es)) = do
    let bts = Vec.map (fmap Eqty.unEquity) (Signal.unSignal es)
        ps' = Vec.map (fmap (O.unOHLC . trdAt)) (Signal.unSignal ps)
        left = (Style.axTitle "Equity", [Rep.line "Symbol at Close" ps', Rep.line "Backtest" bts])
        right = (Style.impulseAxisConf, [Rep.line "down buy / up sell" (Curve.curve impSig)])

    Rep.subheader "Backtest Result"
    Rep.chartLR (Style.axTitle "Time") left right
    Rep.text ("Initial Equity: " ++ show inEq)
    Rep.text ("Starting with equity " ++ show (Vec.head bts))
    Rep.text ("Ending with equity " ++ show (Vec.last bts))

--------------------------------------------------------

example :: IO ()
example = do
  

  let equity = Eqty.Equity 1
      trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OHLC.OHLC OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (IG.optimalBuySell trdAt)
        , Ana.optimizationInput = OptimizationInput ticker
        , Ana.backtestInput = BacktestInput trdAt equity ticker
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
