{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Example.Simple where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as Vec

import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.ToReport as TR

import qualified Trade.Test.Data as TD

import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))

import qualified Trade.Type.ImpulseGenerator as IG
import qualified Trade.Type.ImpulseSignal as IS
import qualified Trade.Type.Strategy as Strat


import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style


ticker :: Signal UTCTime Price
ticker = Signal (Vec.map (fmap Price) TD.test2)
  
--------------------------------------------------------

data OptimizationInput = OptimizationInput (Signal UTCTime Price)

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  type OptInpTy OptimizationInput = Signal UTCTime Price
  
  optimize (IG.ImpulseGenerator strat) (OptimizationInput sig) =
    return (IG.RankedStrategies [strat sig], OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (TR.OptimizationData OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData (OptimizationInput ps) OptimizationResult) = do
    Rep.text "Optimally buying and selling. Not possible in reality :( ..."
    
    Rep.subheader "Optimization Input"
    Rep.chart (Style.axTitle "Symbol") (Style.axTitle "Price", [Line.line "Price" ps])
    Rep.subheader "Optimization Result"
    Rep.text "No optimization has been done."


--------------------------------------------------------

data BacktestInput = BacktestInput {
  initialEquity :: Equity
  , outOfSample :: Signal UTCTime Price
  }

instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult

  backtest (IG.NonEmptyList (IG.OptimizedImpulseGenerator optStrat) _) (BacktestInput initEqty ps) =
    let impSig = optStrat ps
        expmnt = BT.Experiment Strat.Long initEqty impSig ps
        es = BT.equitySignal expmnt
    in (BacktestResult impSig es)

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal UTCTime
  , equities :: Signal UTCTime Equity
  }

instance TR.ToReport (TR.BacktestData BacktestInput BacktestResult) where
  toReport (TR.BacktestData (BacktestInput inEq ps) (BacktestResult impSig es)) = do
    let bts = fmap unEquity es

    Rep.subheader "Backtest Result"
    Rep.text "Trading at full fraction, no commissions"

    Rep.backtestChart
      (Rep.gridChart (Style.axTitle "Equity") [Line.line "Symbol at Close" ps, Line.line "Backtest" bts])
      (Rep.impulseSignalCharts [IS.curve ps impSig])

    
    Rep.text ("Initial Equity: " ++ show inEq)
    Rep.text ("Starting with equity " ++ show (Vec.head $ unSignal bts))
    Rep.text ("Ending with equity " ++ show (Vec.last $ unSignal bts))

--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = Price


instance OD.OHLCData BacktestInput where
  type OHLCDataTy BacktestInput = Price

--------------------------------------------------------

example :: IO ()
example = do
  

  let equity = Equity 10
  
      analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (IG.optimalBuySell unPrice)
        , Ana.optimizationInput = OptimizationInput ticker
        , Ana.backtestInput = BacktestInput equity ticker
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
