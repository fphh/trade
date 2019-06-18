{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Example.Linear where

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


import Trade.Strategy.Library.EvenOdd (evenOdd)
import Trade.Strategy.Type (Window(..))


import Trade.Report.Basic (text, header, subheader)
import Trade.Report.HtmlReader (render)
import qualified Trade.Report.ToReport as TR


import Trade.Report.Config (HtmlReader)

data Symbol = A deriving (Show, Eq, Ord)


barLen :: BarLength
barLen = Day 1

ticker :: Timeseries Price
-- ticker = Signal (Vec.map (fmap Price) TD.testSimple)
ticker = Signal (Vec.map (fmap Price) (Vec.take 7 TD.linear))

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
backtest (NonEmptyList optStrat _) (BacktestInput initEqty bl ps stpL stpS) =
  let expmntLW = Experiment.Input stpL initEqty bl optStrat ps
      esLW = Experiment.conduct expmntLW
      expmntSW = Experiment.Input stpS initEqty bl optStrat ps
      esSW = Experiment.conduct expmntSW
  in (BacktestResult esLW esSW)


instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  
  toReport (ARep.BacktestData _ (BacktestResult resLW resSW)) = do

    header "Backtest Result, Long"
    Experiment.render (const (return ())) resLW

    header "Backtest Result, Short"
    Experiment.render (const (return ())) resSW

--------------------------------------------------------

analyze :: Analysis OptimizationInput BacktestInput () Price -> HtmlReader ()
analyze = analyzeHelper optimize backtest

--------------------------------------------------------


example :: IO ()
example = do

  let equity = Equity 1

      win5 = Window 5
      win10 = Window 10
  
      eo = IG.ImpulseGenerator (const (IG.OptimizedImpulseGenerator evenOdd))
      
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
        title = "Linear even/odd"
        , impulseGenerator = gen
        , optimizationInput = OptimizationInput
        , backtestInput = BacktestInput equity barLen (Map.fromList [(A, ticker)]) stpL stpS
        }

      repA = analyze (analysis eo)

  a <- render repA
  BSL.putStrLn a

