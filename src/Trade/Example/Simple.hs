{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Example.Simple where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as Vec

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Analysis.Analysis (analyzeHelper, Analysis(..))
import qualified Trade.Analysis.Report as ARep

import qualified Trade.Test.Data as TD

import Trade.Type.BarLength (BarLength(Day))

import Trade.Type.Step (StepTy(LongStep, ShortStep), longFraction, shortFraction, longCommission, shortCommission, shortInterests)
import Trade.Type.Step.Commission (noCommission)
import Trade.Type.Step.Fraction (fullFraction)
import Trade.Type.Step.Interests (Interests(..), interests)

import Trade.Type.Equity (Equity(..))
import Trade.Type.NonEmptyList (NonEmptyList(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Timeseries, Signal(..))
import Trade.Type.Strategy (Long, Short)

import qualified Trade.Type.Experiment as Experiment
import qualified Trade.Type.ImpulseGenerator as IG


import Trade.Strategy.Library.MovingAverages (movingAverages)
import Trade.Strategy.Type (Window(..))


import Trade.Report.Basic (text, header, subheader)
import Trade.Report.HtmlReader (render)
import qualified Trade.Report.ToReport as TR


import Trade.Report.Config (HtmlReader)

data Symbol = A deriving (Show, Eq, Ord)

barLen :: BarLength
barLen = Day 1

ticker :: Signal UTCTime Price
-- ticker = Signal (Vec.map (fmap Price) TD.test2)
ticker = Signal (Vec.map (fmap Price) TD.sinus)
-- ticker = Signal (Vec.map (fmap Price) TD.linear)

--------------------------------------------------------


data OptimizationInput = OptimizationInput
data OptimizationResult = OptimizationResult

optimize ::
  IG.ImpulseGenerator () ohlc
  -> OptimizationInput
  -> (IG.RankedStrategies ohlc, OptimizationResult)
optimize (IG.ImpulseGenerator strat) OptimizationInput =
  (IG.RankedStrategies [strat ()], OptimizationResult)

instance TR.ToReport (ARep.OptimizationData OptimizationInput OptimizationResult) where
  toReport (ARep.OptimizationData OptimizationInput OptimizationResult) = do
    subheader "Optimization Result"
    text "No optimization has been done."

--------------------------------------------------------

data BacktestInput = BacktestInput {
  initialEquity :: Equity
  , barLength :: BarLength
  , symbol :: Symbol
  , outOfSample :: Map Symbol (Timeseries Price)
  , stepLong :: StepTy Long
  , stepShort :: StepTy Short
  }

data BacktestResult = BacktestResult {
  resultLW :: Experiment.Result Long Symbol Price
  , resultSW :: Experiment.Result Short Symbol Price
  }


backtest ::
  NonEmptyList (IG.OptimizedImpulseGenerator Price)
  -> BacktestInput
  -> BacktestResult
backtest (NonEmptyList optStrat _) (BacktestInput initEqty bl sym ps stpL stpS) =
  let configLW = Experiment.Config stpL initEqty  bl
      expmntLW = Experiment.Input sym optStrat ps
      esLW = Experiment.conduct expmntLW configLW

      configSW = Experiment.Config stpS initEqty  bl
      expmntSW = Experiment.Input sym optStrat ps
      esSW = Experiment.conduct expmntSW configSW
  in (BacktestResult esLW esSW)


instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  
  toReport (ARep.BacktestData (BacktestInput initEqty bl _ _ stpL stpS) (BacktestResult resLW resSW)) = do

    let configLW = Experiment.Config stpL initEqty  bl
    header "Backtest Result, Long"
    Experiment.render configLW (const (return ())) resLW

    let configSW = Experiment.Config stpS initEqty  bl
    header "Backtest Result, Short"
    Experiment.render configSW (const (return ())) resSW

--------------------------------------------------------

analyze :: Analysis OptimizationInput BacktestInput () Price -> HtmlReader ()
analyze = analyzeHelper optimize backtest

--------------------------------------------------------


example :: IO ()
example = do

  let equity = Equity 3

      win5 = Window 5
      win10 = Window 10
  
      gen_5_10 = IG.ImpulseGenerator (const (movingAverages win5 win10))
      gen_10_5 = IG.ImpulseGenerator (const (movingAverages win10 win5))
      
      rtf dt =
          let day = 24*60*60
          in realToFrac dt / day


      stpL = LongStep {
        longFraction = fullFraction
        , longCommission = noCommission
        }

      stpS = ShortStep {
        shortFraction = fullFraction
        , shortCommission = noCommission
        , shortInterests = Interests (interests rtf 0)
        }


      analysis :: IG.ImpulseGenerator () Price -> Analysis OptimizationInput BacktestInput () Price
      analysis gen = Analysis {
        title = "Long/Short - Winning/Losing"
        , impulseGenerator = gen
        , optimizationInput = OptimizationInput
        , backtestInput = BacktestInput equity barLen A (Map.fromList [(A, ticker)]) stpL stpS
        }

      repA = analyze (analysis gen_5_10)
      repB = analyze (analysis gen_10_5)

  a <- render repA
  BSL.putStrLn a

  b <- render repB
  BSL.putStrLn b

