{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}


module Trade.Example.Simple where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import qualified Trade.Type.Equity as Eqty
import qualified Trade.Type.OHLC as O

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.ImpulseSignal as IS
import qualified Trade.Type.Signal.Equity as ES
import qualified Trade.Type.StepFunc as SF

import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.OHLCData as OD

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Curve as Curve
import qualified Trade.Report.Line as Line

import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


ticker :: Signal.Signal UTCTime OHLC.OHLC
ticker =
  let f x = OHLC.OHLC (O.Open (x+0.5)) (O.High (x+1)) (O.Low (x-1)) (O.Close x) (O.Volume 1000)
  in Signal.Signal (Vec.map (fmap f) TD.test2)
  
--------------------------------------------------------

data OptimizationInput = OptimizationInput (Signal.Signal UTCTime OHLC.OHLC)

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  type OptInpTy OptimizationInput = Signal.Signal UTCTime OHLC.OHLC
  
  optimize (IG.ImpulseGenerator strat) (OptimizationInput sig) =
    return (strat sig, OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OptimizationInput OptimizationResult) where
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
  , outOfSample :: Signal.Signal UTCTime ohlc
  }
    
instance BT.Backtest (BacktestInput ohlc) where
  type BacktestReportTy (BacktestInput ohlc) = BacktestResult

  backtest (IG.OptimizedImpulseGenerator optStrat) (BacktestInput trdAt initEqty ps) =
    let impSig = optStrat ps
        es = BT.equitySignal trdAt SF.stepFuncNoCommissionFullFraction initEqty impSig ps
    in (BacktestResult impSig es)

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal UTCTime
  , eqties :: ES.EquitySignal UTCTime
  }

instance TR.ToReport (TR.BacktestData (BacktestInput ohlc) BacktestResult) where
  toReport (TR.BacktestData (BacktestInput trdAt inEq ps) (BacktestResult impSig es)) = do
    let bts = fmap Eqty.unEquity es
        ps' = fmap (O.unOHLC . trdAt) ps
        left = (Style.axTitle "Equity", [Line.line "Symbol at Close" ps', Line.line "Backtest" bts])
        right = (Style.impulseAxisConf, [Line.line "down buy / up sell" (IS.curve ps impSig)])

    Rep.subheader "Backtest Result"
    Rep.text "Trading at full fraction, no commissions"

    Rep.chartLR (Style.axTitle "Time") left right
    Rep.text ("Initial Equity: " ++ show inEq)
    Rep.text ("Starting with equity " ++ show (Vec.head $ Signal.unSignal bts))
    Rep.text ("Ending with equity " ++ show (Vec.last $ Signal.unSignal bts))

    Rep.text (show (Signal.length ps))
    Rep.text (show (Signal.length es))

--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = OHLC.OHLC

instance OD.OHLCData (BacktestInput ohlc) where
  type OHLCDataTy (BacktestInput ohlc) = ohlc

--------------------------------------------------------

example :: IO ()
example = do
  

  let equity = Eqty.Equity 1
      trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OptimizationInput (BacktestInput OHLC.OHLC)
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (IG.optimalBuySell trdAt)
        -- , Ana.impulseGenerator = IG.optImpGen2impGen (IG.buyAtSellAtAbs 15 18)
        , Ana.optimizationInput = OptimizationInput ticker
        , Ana.backtestInput = BacktestInput trdAt equity ticker
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
