{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-

* GARCH models
* Interactive Brokers
  https://github.com/rbermani/ib-api

* quantopian: zipline
* Johansen Test for Cointegrating Time Series Analysis in R

* https://hackage.haskell.org/package/inline-r

-}

module Trade.Example.Optimize where

import Control.Monad.Trans (liftIO)
import Control.Monad (replicateM)
import Control.DeepSeq (force, NFData, rnf)

import Control.Applicative (liftA2)

import Data.Time.Clock (UTCTime)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as Easy

import qualified Data.Vector as Vec

import Text.Printf (printf)

import qualified Text.Blaze.Html5 as H5

import qualified Trade.Type.Impulse as Imp
import qualified Trade.Type.Equity as E
import qualified Trade.Type.OHLC as O
import qualified Trade.Type.Price as P
import qualified Trade.Type.Bars as B
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Strategy as Strat
import qualified Trade.Type.Broom as Broom
import qualified Trade.Type.Distribution as Dist
import qualified Trade.Type.Fraction as F
import qualified Trade.Type.Trade as Trade
import qualified Trade.Type.Yield as Y

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.ImpulseSignal as IS
import qualified Trade.Type.Signal.Equity as ES

import qualified Trade.Type.Conversion.Price2Yield as P2Y
import qualified Trade.Type.Conversion.Type2Double as T2D

import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Timeseries.OHLC as OHLC
import qualified Trade.Timeseries.Algorithm.Intersection as Inter

import qualified Trade.Algorithm.MovingAverage as MAvg

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.TWR as TWR
import qualified Trade.Analysis.Risk as Risk
import qualified Trade.Analysis.OHLCData as OD

import qualified Trade.TStatistics.SampleStatistics as SStat
import qualified Trade.TStatistics.TradeStatistics as TStat

import qualified Trade.MonteCarlo.ResampleTrades.Broom as RTBroom


import qualified Trade.MonteCarlo.Simulation.BlackScholes as Black
import qualified Trade.MonteCarlo.Simulation.MEBoot as MEBoot

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Curve as Curve
import qualified Trade.Report.Line as Line

import qualified Trade.Test.Time as T
import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


import Debug.Trace

--------------------------------------------------------

type Steps = SF.StepFunc Y.Yield

--------------------------------------------------------

data OptimizationInput t ohlc = OptimizationInput {
  optSample :: Signal.Signal t ohlc
  , optSpace :: [(IG.Percent, MAvg.WindowSize)]
  , riskOfLoss :: Double
  , mcN :: Int
  , optInitialEquity :: E.Equity
  , forcastHorizon :: B.Bars
  , stepFunc :: F.Fraction -> Steps
  , fractions :: [F.Fraction]
  }

data OptSingleResult = OptSingleResult {
  ig :: IG.OptimizedImpulseGenerator P.Price
  , params :: (IG.Percent, MAvg.WindowSize)
  , twrTable10 :: ![(F.Fraction, Maybe Dist.Percent)]
  , twrTable12 :: ![(F.Fraction, Maybe Dist.Percent)]
  , rsks :: ![(F.Fraction, Maybe Dist.Percent)]
  }

instance NFData F.Fraction where
  rnf (F.Fraction !x) = ()

instance Opt.Optimize (OptimizationInput UTCTime P.Price) where
  type OptReportTy (OptimizationInput UTCTime P.Price) = OptimizationResult
  type OptInpTy (OptimizationInput UTCTime P.Price) = (IG.Percent, MAvg.WindowSize)
  
  optimize (IG.ImpulseGenerator strat) optInp = do
    let len = Signal.length (optSample optInp)
        
        yields :: Signal.Signal UTCTime Y.LogYield
        yields = P2Y.price2yield (optSample optInp)
        sampleStats = SStat.sampleStatistics yields

        mu = Black.Mu (fromIntegral len * SStat.mean sampleStats)
        sigma = Black.Sigma (sqrt (fromIntegral len) * SStat.stdDev sampleStats)

    --  priceBrm <- Black.priceSignalBroom (forcastHorizon optInp) (mcN optInp) (optInitialEquity optInp) mu sigma

    priceBrm <- MEBoot.mebootBroom (mcN optInp) (optSample optInp)

    let eqty = optInitialEquity optInp
        fracs = fractions optInp
        space = optSpace optInp
    
        f params@(perc, winSize) =
          let optIG@(IG.OptimizedImpulseGenerator optStrat) = strat params
              impBrm = fmap optStrat priceBrm

              wealthAndRisk fr (ts, rs) =
                let sf = stepFunc optInp fr
                    g = BT.Experiment Strat.Long id sf eqty
                    eqtyBroom = Broom.zipWith (\is ps -> BT.equitySignal (g is ps)) impBrm priceBrm
                    tw = TWR.terminalWealthRelative eqty eqtyBroom
                    rk = Risk.risk eqtyBroom
                in ((fr, tw):ts, (fr, rk):rs)

              (twrs, rsks) = List.foldr wealthAndRisk ([], []) fracs
                
              g mdf cmp (F.Fraction fr, Dist.CDF vs) =
                (F.Fraction fr, fmap (mdf . fst) (Vec.find (cmp . snd) vs))

              h (F.Fraction fr, Dist.CDF vs) =
                (F.Fraction fr, fmap ((1-) . fst) (Vec.find ((>0.2) . snd) vs))
    
              twr10 = map (g id (not . (<= 1.0))) twrs
              twr12 = map (g (1-) (> 1.2)) twrs
              rsk20 = map h rsks
          
          in OptSingleResult optIG params (force twr10) (force twr12) (force rsk20)

        res = map f space

        h ps ig (fr, b) (_, c) = (fr, b, c, ps, ig)
        g (OptSingleResult optIG params a b c) = zipWith (h params optIG) a b

        p (_, x, _, _, _) =
          case x of
            Nothing -> True
            Just y | y < (riskOfLoss optInp) -> True
            _ -> False

        q (_, _, u, _, _) (_, _, v, _, _) = compare u v
        (optFr, _, _, (optPerc, optWinSize), optimalIG) =
          List.maximumBy q (filter p (concatMap g res))

    return (IG.RankedStrategies [optimalIG], OptimizationResult priceBrm sampleStats mu sigma res (optFr, optPerc, optWinSize))


data OptimizationResult = OptimizationResult {
  broom :: Broom.Broom (Signal.Signal UTCTime P.Price)
  , sampleStats :: SStat.SampleStatistics UTCTime
  , muOR :: Black.Mu
  , sigmaOR :: Black.Sigma
  , optSingleResults :: [OptSingleResult]
  , best :: (F.Fraction, IG.Percent, MAvg.WindowSize)
  }

instance (T2D.Type2Double ohlc) =>
         TR.ToReport (TR.OptimizationData (OptimizationInput UTCTime ohlc) OptimizationResult) where
  
  toReport (TR.OptimizationData optInp (OptimizationResult brm sStats mu sigma singleRes bst)) = do
    let nOfSamp = 20

    Rep.text "Trying to optimize a mean reversion strategy. Parameters are window size and percent of deviation from current prices. Trying to identify parameters with low risk and high yield. The Monte Carlo simulation is based on Black Scholes."
    
    Rep.subheader "Optimization Input"
    Rep.chart (Style.axTitle "Symbol") (Style.axTitle "Price", [Line.line "Price" (optSample optInp)])

    Rep.subsubheader "Sample Statistics of Log Yields"
    SStat.stats2para sStats
    
    Rep.subsubheader "Generated Broom"
    Rep.text ("Black-Scholes with parameters: " ++ show mu ++ ", " ++ show sigma)
    Rep.text ("Number of Monte Carlo samples: " ++ show (mcN optInp) ++ ", showing " ++ show nOfSamp)
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Price", Broom.broom2chart nOfSamp brm)

    Rep.subheader "Optimization Result"
    
    let format = maybe "n/a" (printf "%.02f%%" . (100*))
        showFrac (F.Fraction fr) = printf "Frac %.02f" fr

        f (OptSingleResult _ params b c d) = do
          let g (fr, b) (_, c) (_, d) = [ showFrac fr, format b, format c, format d ]
              headerFormat (IG.Percent p, MAvg.WindowSize ws) =
                printf "Moving Window %d bars, triggering at %.02f%% deviation from actual price" ws (100*p)
          
          Rep.subsubheader (headerFormat params)
          Rep.htable (["Fraction f", "P(TWR <= 1)", "P(TWR > 1.2)", "P(max. drawdown > 20%)"] : zipWith3 g b c d)

    mapM_ f singleRes

    Rep.subheader "Optimization Result"
    Rep.text ("Best is " ++ show bst)
    
--------------------------------------------------------

data BacktestInput t ohlc = BacktestInput {
  initialEquity :: E.Equity
  , pricesInput :: Signal.Signal t ohlc
  }
    
instance (Ord t, Show t, Show ohlc, B.Time t, Num (B.DeltaT t), T2D.Type2Double ohlc, Show (B.DeltaT t)) =>
         BT.Backtest (BacktestInput t ohlc) where
  
  type BacktestReportTy (BacktestInput t ohlc) = BacktestResult t

  backtest (IG.NonEmptyList (IG.OptimizedImpulseGenerator optStrat) _) (BacktestInput initEqty ps) =
    let impSig = optStrat ps
        sf :: Steps
        sf =  SF.stepFuncNoCommissionFullFraction
        exp = BT.Experiment Strat.Long id sf initEqty impSig ps
        es = BT.equitySignal exp
    in BacktestResult impSig es

data BacktestResult t = BacktestResult {
  impulses :: IS.ImpulseSignal t
  , eqties :: ES.EquitySignal t
  }

instance (Easy.PlotValue t, Show t, T2D.Type2Double ohlc) =>
         TR.ToReport (TR.BacktestData (BacktestInput t ohlc) (BacktestResult t)) where
  toReport (TR.BacktestData (BacktestInput inEq ps) (BacktestResult impSig es)) = do
    
    Rep.subheader "Backtest Result"

    Rep.backtestChart
      (Rep.gridChart (Style.axTitle "Equity") [Line.line "Equity" ps, Line.line "Backtest" es])
      (Rep.impulseSignalCharts [IS.curve ps impSig])

    
    Rep.text ("Initial Equity: " ++ show inEq)

    case Signal.length es > 0 of
      True -> do
        Rep.text ("Starting with equity " ++ show (Signal.head es))
        Rep.text ("Ending with equity " ++ show (Signal.last es))
      False -> do
        Rep.text "No trades occured"

        
--------------------------------------------------------

instance OD.OHLCData (OptimizationInput t price) where
  type OHLCDataTy (OptimizationInput t price) = price

instance OD.OHLCData (BacktestInput t price) where
  type OHLCDataTy (BacktestInput t price) = price

--------------------------------------------------------

example :: IO ()
example = do
  
  let mu = Black.Mu 0.5
      sigma = Black.Sigma 0.5
      start = E.Equity 100
      seed = 100

  samp <- Black.blackScholesDet seed (T.yearsN 4) start mu sigma
  
  let Signal.Sample inSample outOfSample = Signal.split 0.75 samp

      initEq = E.Equity (P.unPrice (snd (Signal.head outOfSample)))

      percs = map IG.Percent [0, 0.01, 0.02, 0.03, 0.04, 0.05]
      winSizes = map MAvg.WindowSize [5, 10, 15, 20]
      optSpc = liftA2 (,) percs winSizes

      -- ig = IG.ImpulseGenerator (uncurry (IG.impulsesFromMovingAverage P.unPrice))
      ig = IG.impulsesFromMovingAverage P.unPrice

      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = ig
        , Ana.optimizationInput = OptimizationInput {
            optSample = inSample
            , optSpace = optSpc
            , riskOfLoss = 0.1
            , mcN = 1000
            , optInitialEquity = E.Equity 1000
            , forcastHorizon = B.Bars 1000
            , stepFunc = SF.stepFuncNoCommission
            , fractions = map F.Fraction [0.1, 0.5, 1, 1.5, 2.0, 5.0]
            }
        , Ana.backtestInput = BacktestInput initEq outOfSample
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
