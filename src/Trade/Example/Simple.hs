{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Example.Simple where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as Vec

import Data.Map (Map)

import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.Report as ARep

import qualified Trade.Test.Data as TD

import Trade.Type.Step (StepTy(LongStep, ShortStep), longFraction, shortFraction, longCommission, shortCommission, shortInterests)
import Trade.Type.Step.Commission (Commission(..), noCommission)
import Trade.Type.Step.Fraction (Fraction(..), fullFraction)
import Trade.Type.Step.Interests (Interests(..), interests)

import Trade.Type.DisInvest (InvestSignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Impulse (invert)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.NonEmptyList (NonEmptyList(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Strategy (Long, Short)

import qualified Trade.Type.Experiment as Experiment
import qualified Trade.Type.ImpulseGenerator as IG


import Trade.Strategy.Library.BuyAndHold (buyAndHold)
import Trade.Strategy.Library.MovingAverages (movingAverages)
import Trade.Strategy.Type (Window(..))
import qualified Trade.Strategy.Process as Strategy

import Trade.Report.Line (Line(..))
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import qualified Trade.Report.ToReport as TR

import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))


data Symbol = A deriving (Show, Eq, Ord)

ticker :: Signal UTCTime Price
-- ticker = Signal (Vec.map (fmap Price) TD.testSimple)
ticker = Signal (Vec.map (fmap Price) TD.sinus)

--------------------------------------------------------


data OptimizationInput = OptimizationInput [(Symbol, Signal UTCTime Price)]

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  type OptInpTy OptimizationInput = ()

  optimize (IG.ImpulseGenerator strat) (OptimizationInput sig) =
    (IG.RankedStrategies [strat ()], OptimizationResult)


data OptimizationResult = OptimizationResult

instance TR.ToReport (ARep.OptimizationData OptimizationInput OptimizationResult) where
  toReport (ARep.OptimizationData (OptimizationInput ((_, ps):_)) OptimizationResult) = do
    Rep.text "Optimally buying and selling. Not possible in reality :( ..."
    
    Rep.subheader "Optimization Input"
    Rep.chart
      (Style.axTitle "Symbol" "Time" :: Style.AxisConfig UTCTime Price)
      (Style.axTitle "Price" "Time" :: Style.AxisConfig Price UTCTime, [Line "Price" ps])
    Rep.subheader "Optimization Result"
    Rep.text "No optimization has been done."

--------------------------------------------------------

data BacktestInput = BacktestInput {
  initialEquity :: Equity
  , outOfSample :: [(Symbol, Signal UTCTime Price)]
  }

instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult

  backtest (NonEmptyList optStrat@(IG.OptimizedImpulseGenerator strat) _) (BacktestInput initEqty ps) =

    let rtf dt =
          let day = 24*60*60
          in realToFrac dt / day


        stp0 = LongStep {
          longFraction = Fraction 0.5
          , longCommission = Commission (const 0) -- (\c -> 0.05*c)
          }

        stp1 = ShortStep {
          shortFraction = Fraction 0.5
          , shortCommission = Commission (const 0) -- (\c -> 0.05*c)
          , shortInterests = Interests (interests rtf 0)
          }

        -- expmntLW :: Experiment.Input Long Symbol UTCTime Price
        expmntLW = Experiment.Input stp0 initEqty optStrat ps
        esLW = Experiment.conduct expmntLW

        expmntSW = Experiment.Input stp1 initEqty optStrat ps
        esSW = Experiment.conduct expmntSW

        expmntLL = Experiment.Input stp0 initEqty optStrat ps
        esLL = Experiment.conduct expmntLL

        expmntSL = Experiment.Input stp1 initEqty optStrat ps
        esSL = Experiment.conduct expmntSL


    in (BacktestResult esLW esSW esLL esSL)

data BacktestResult = BacktestResult {
  resultLW :: Experiment.Result Long Symbol UTCTime Price
  , resultSW :: Experiment.Result Short Symbol UTCTime Price
  , resultLL :: Experiment.Result Long Symbol UTCTime Price
  , resultSL :: Experiment.Result Short Symbol UTCTime Price
  }

instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  toReport (ARep.BacktestData (BacktestInput inEq ps) (BacktestResult resLW resSW resLL resSL)) = do

    Rep.subheader "Fees"

    Rep.text "Trading at fraction 0.5, commission per buy/sell 5%, short interests 2% per day."

    Rep.header "Backtest Result, Long, Winning"
    Experiment.render "Symbol at Close" "Backtest" resLW

    Rep.header "Backtest Result, Short, Winning"
    Experiment.render "Symbol at Close" "Backtest" resSW
{-
    Rep.header "Backtest Result, Long, Losing"
    Experiment.render "Symbol at Close" "Backtest" resLL

    Rep.header "Backtest Result, Short, Losing"
    Experiment.render "Symbol at Close" "Backtest" resSL
-}

--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = Price

instance OD.OHLCData BacktestInput where
  type OHLCDataTy BacktestInput = Price

--------------------------------------------------------


example :: IO ()
example = do

  let equity = Equity 3
  
      analysis :: Ana.Analysis OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "Long/Short - Winning/Losing"
        -- , Ana.impulseGenerator = IG.ImpulseGenerator (\() -> IG.OptimizedImpulseGenerator buyAndHold)
        , Ana.impulseGenerator = IG.ImpulseGenerator (\() -> IG.OptimizedImpulseGenerator (movingAverages (Window 5) (Window 10)))
        , Ana.optimizationInput = OptimizationInput [(A, ticker)]
        , Ana.backtestInput = BacktestInput equity [(A, ticker)]
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
