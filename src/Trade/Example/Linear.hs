{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Example.Linear where

import Data.Time.Clock (UTCTime, NominalDiffTime)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as Vec

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.Report as ARep

import qualified Trade.Test.Data as TD

import Trade.Type.Bars (BarLength(Day), barLength2diffTime)

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
import Trade.Strategy.Library.EvenOdd (evenOdd)
import Trade.Strategy.Library.MovingAverages (movingAverages)
import Trade.Strategy.Type (Window(..))
import qualified Trade.Strategy.Process as Strategy

import Trade.Report.Line (Line, line)


import Trade.Report.Basic (text, header, subheader)
import qualified Trade.Report.Chart as Chart
import Trade.Report.HtmlReader (render)
import qualified Trade.Report.Style as Style
import qualified Trade.Report.ToReport as TR

import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))


data Symbol = A deriving (Show, Eq, Ord)

barLen :: BarLength
barLen = Day 1

ticker :: Signal UTCTime Price
-- ticker = Signal (Vec.map (fmap Price) TD.testSimple)
ticker = Signal (Vec.map (fmap Price) (Vec.take 7 TD.linear))

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
    text "Optimally buying and selling. Not possible in reality :( ..."
    
    subheader "Optimization Input"
    Chart.lines
      (Style.axTitle "Symbol" "Time" :: Style.AxisConfig UTCTime Price)
      (Style.axTitle "Price" "Time" :: Style.AxisConfig Price UTCTime, [line "Price" ps])
    subheader "Optimization Result"
    text "No optimization has been done."

--------------------------------------------------------

data BacktestInput = BacktestInput {
  initialEquity :: Equity
  , barLength :: NominalDiffTime
  , outOfSample :: Map Symbol (Signal UTCTime Price)
  }

instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult

  backtest (NonEmptyList optStrat@(IG.OptimizedImpulseGenerator strat) _) (BacktestInput initEqty bl ps) =

    let rtf dt =
          let day = 24*60*60
          in realToFrac dt / day


        stp0 = LongStep {
          longFraction = Fraction 1
          , longCommission = Commission (const 0) -- (\c -> 0.05*c)
          }

        stp1 = ShortStep {
          shortFraction = Fraction 1
          , shortCommission = Commission (const 0) -- (\c -> 0.05*c)
          , shortInterests = Interests (interests rtf 0)
          }


        expmntLW = Experiment.Input stp0 initEqty bl optStrat ps
        esLW = Experiment.conduct expmntLW

        expmntSW = Experiment.Input stp1 initEqty bl optStrat ps
        esSW = Experiment.conduct expmntSW

    in (BacktestResult esLW esSW)


data BacktestResult = BacktestResult {
  resultLW :: Experiment.Result Long Symbol UTCTime Price
  , resultSW :: Experiment.Result Short Symbol UTCTime Price
  }


instance TR.ToReport (ARep.BacktestData BacktestInput BacktestResult) where
  
  toReport (ARep.BacktestData (BacktestInput inEq _ ps) (BacktestResult resLW resSW)) = do

    header "Backtest Result, Long"
    Experiment.render resLW

--    header "Backtest Result, Short"
--    Experiment.render resSW

    -- mapM_ text (map show (Vec.toList (Vec.map show (unSignal $ Experiment.outputSignal (Experiment.output resLW)))))

--------------------------------------------------------

instance OD.OHLCData OptimizationInput where
  type OHLCDataTy OptimizationInput = Price

instance OD.OHLCData BacktestInput where
  type OHLCDataTy BacktestInput = Price

--------------------------------------------------------


example :: IO ()
example = do

  let equity = Equity 1

      win5 = Window 5
      win10 = Window 10
  
      gen_5_10 = IG.ImpulseGenerator (const (IG.OptimizedImpulseGenerator evenOdd))

      analysis :: IG.ImpulseGenerator () Price -> Ana.Analysis OptimizationInput BacktestInput
      analysis gen = Ana.Analysis {
        Ana.title = "Long/Short - Winning/Losing"
        , Ana.impulseGenerator = gen
        , Ana.optimizationInput = OptimizationInput [(A, ticker)]
        , Ana.backtestInput = BacktestInput equity (barLength2diffTime barLen) (Map.fromList [(A, ticker)])
        }

      repA = Ana.analyze (analysis gen_5_10)

  a <- render repA
  BSL.putStrLn a
