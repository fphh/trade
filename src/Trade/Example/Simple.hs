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

import Trade.Type.Step (StepTy(LongStep, ShortStep), longFraction, shortFraction, longCommission, shortCommission, shortInterests)
import Trade.Type.Step.Commission (Commission(..), noCommission)
import Trade.Type.Step.Fraction (Fraction(..), fullFraction)
import Trade.Type.Step.Interests (Interests(..), interests)

import Trade.Type.Bars (DeltaTy(NDT))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Impulse (invert)
import Trade.Type.NonEmptyList (NonEmptyList(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Strategy (Long, Short)

import qualified Trade.Type.Experiment as Experiment
import qualified Trade.Type.ImpulseGenerator as IG


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

  backtest (NonEmptyList optStrat@(IG.OptimizedImpulseGenerator strat) _) (BacktestInput initEqty ps) =

    let rtf dt =
          let day = 24*60*60
          in realToFrac dt / day

        stp0 = LongStep {
          longFraction = Fraction 0.5
          , longCommission = Commission (\c -> 0.05*c)
          }

        stp1 = ShortStep {
          shortFraction = Fraction 0.5
          , shortCommission = Commission (\c -> 0.05*c)
          , shortInterests = Interests (interests rtf 0.02)
          }

        expmntLW = Experiment.Input stp0 initEqty optStrat ps
        esLW = Experiment.conduct expmntLW

        expmntSW = Experiment.Input stp1 initEqty optStrat ps
        esSW = Experiment.conduct expmntSW

        expmntLL = Experiment.Input stp0 initEqty (IG.invertOpt optStrat) ps
        esLL = Experiment.conduct expmntLL

        expmntSL = Experiment.Input stp1 initEqty (IG.invertOpt optStrat) ps
        esSL = Experiment.conduct expmntSL

    in (BacktestResult esLW esSW esLL esSL)

data BacktestResult = BacktestResult {
  resultLW :: Experiment.Result Long UTCTime Price
  , resultSW :: Experiment.Result Short UTCTime Price
  , resultLL :: Experiment.Result Long UTCTime Price
  , resultSL :: Experiment.Result Short UTCTime Price
  }

instance TR.ToReport (TR.BacktestData BacktestInput BacktestResult) where
  toReport (TR.BacktestData (BacktestInput inEq ps) (BacktestResult resLW resSW resLL resSL)) = do

    Rep.subheader "Fees"

    Rep.text "Trading at fraction 0.5, commission per buy/sell 5%, short interests 2% per day."

    Rep.subheader "Backtest Result, Long, Winning"
    Experiment.render "Symbol at Close" "Backtest" resLW

    Rep.subheader "Backtest Result, Short, Winning"
    Experiment.render "Symbol at Close" "Backtest" resSW

    Rep.subheader "Backtest Result, Long, Losing"
    Experiment.render "Symbol at Close" "Backtest" resLL

    Rep.subheader "Backtest Result, Short, Losing"
    Experiment.render "Symbol at Close" "Backtest" resSL


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
