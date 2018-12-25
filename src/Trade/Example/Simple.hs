{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Example.Simple where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import qualified Trade.Type.Equity as Eqty

import qualified Trade.Type.Fraction as F

import qualified Trade.Type.OHLC as O

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.ImpulseSignal as IS
import qualified Trade.Type.Signal.Equity as ES
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Strategy as Strat
import qualified Trade.Type.ImpulseGenerator as IG
import qualified Trade.Type.Yield as Y

import qualified Trade.Type.Conversion.Type2Double as T2D


import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.OHLCData as OD

import qualified Trade.Report.Report as Rep
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
    return (IG.RankedStrategies [strat sig], OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData (OptimizationInput ps) OptimizationResult) = do
    let toC (t, ohlc) =
          let c = E.Candle t
                (T2D.type2double $ OHLC.ohlcLow ohlc)
                (T2D.type2double $ OHLC.ohlcOpen ohlc)
                0
                (T2D.type2double $ OHLC.ohlcClose ohlc)
                (T2D.type2double $ OHLC.ohlcHigh ohlc)
          in c
        toCandle (Signal.Signal cs) = Vec.map toC cs

    Rep.text "Optimally buying and selling. Not possible in reality :( ..."
    
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
    
instance (Show ohlc) => BT.Backtest (BacktestInput ohlc) where
  type BacktestReportTy (BacktestInput ohlc) = BacktestResult

  backtest (IG.NonEmptyList (IG.OptimizedImpulseGenerator optStrat) _) (BacktestInput trdAt initEqty ps) =
    let impSig = optStrat ps
        sf :: SF.StepFunc Y.Yield
        -- sf = SF.stepFuncNoCommissionFullFraction
        sf = SF.stepFuncRelativePrice 0.07 (F.Fraction 0.5)
        expmnt = BT.Experiment Strat.Long trdAt sf initEqty impSig ps
        es = BT.equitySignal expmnt
    in (BacktestResult impSig es)

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal UTCTime
  , eqties :: ES.EquitySignal UTCTime
  }

instance TR.ToReport (TR.BacktestData (BacktestInput ohlc) BacktestResult) where
  toReport (TR.BacktestData (BacktestInput trdAt inEq ps) (BacktestResult impSig es)) = do
    let bts = fmap Eqty.unEquity es
        ps' = fmap (T2D.type2double . trdAt) ps

    Rep.subheader "Backtest Result"
    Rep.text "Trading at full fraction, no commissions"

    Rep.backtestChart
      (Rep.gridChart (Style.axTitle "Equity") [Line.line "Symbol at Close" ps', Line.line "Backtest" bts])
      (Rep.impulseSignalCharts [IS.curve ps impSig])

    
    Rep.text ("Initial Equity: " ++ show inEq)
    Rep.text ("Starting with equity " ++ show (Vec.head $ Signal.unSignal bts))
    Rep.text ("Ending with equity " ++ show (Vec.last $ Signal.unSignal bts))

--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = OHLC.OHLC

instance OD.OHLCData (BacktestInput ohlc) where
  type OHLCDataTy (BacktestInput ohlc) = ohlc

--------------------------------------------------------

example :: IO ()
example = do
  

  let equity = Eqty.Equity 100
      trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OptimizationInput (BacktestInput OHLC.OHLC)
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = (IG.optImpGen2impGen (IG.optimalBuySell trdAt))
        , Ana.optimizationInput = OptimizationInput ticker
        , Ana.backtestInput = BacktestInput trdAt equity ticker
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
