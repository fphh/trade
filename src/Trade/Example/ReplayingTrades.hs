{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Example.ReplayingTrades where

import Data.Time.Clock (UTCTime)

import qualified Data.List as List

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import Text.Printf (printf)

import qualified Trade.Type.Equity as Eqty
import qualified Trade.Type.Price as P

import qualified Trade.Type.Bars as B
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Broom as Broom
import qualified Trade.Type.Distribution as Dist
import qualified Trade.Type.Fraction as F
import qualified Trade.Type.Trade as Trade
import qualified Trade.Type.Yield as Y

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Equity as ES
import qualified Trade.Type.ImpulseSignal as IS


import qualified Trade.Type.Conversion.Impulse2Trade as I2T
import qualified Trade.Type.Conversion.Trade2NormTrade as T2NT


import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Timeseries.OHLC as OHLC

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

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Line as Line

import qualified Trade.Test.Time as T
-- import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style

data OptimizationInput t ohlc = OptimizationInput {
  optSample :: Signal.Signal t ohlc
  , mcN :: Int
  , optInitialEquity :: Eqty.Equity
  , forcastHorizon :: B.Bars
  , stepFunc :: F.Fraction -> SF.StepFunc Y.Yield
  , fractions :: [F.Fraction]
  }
  

instance Opt.Optimize (OptimizationInput UTCTime P.Price) where
  type OptReportTy (OptimizationInput UTCTime P.Price) = OptimizationResult
  type OptInpTy (OptimizationInput UTCTime P.Price) = ()

  optimize (IG.ImpulseGenerator strat) optInp = do
    let optIG@(IG.OptimizedImpulseGenerator optStrat) = strat ()

        trds = I2T.impulse2trade (optSample optInp) (optStrat (optSample optInp))
        ntrds = T2NT.trade2normTrade trds
    yieldBroom <- RTBroom.normBroom (forcastHorizon optInp) (mcN optInp) ntrds

    let sf = stepFunc optInp
        eq = optInitialEquity optInp

        eqtyBrm = Broom.yield2equity (sf (F.Fraction 1)) eq yieldBroom
    
        f fr (ts, rs) =
          let eb = Broom.yield2equity (sf fr) eq yieldBroom
              tw = TWR.terminalWealthRelative eq eb
              rk = Risk.risk eb
          in ((fr, tw):ts, (fr, rk):rs)
          
        (twrs, rsks) = List.foldr f ([], []) (fractions optInp)

    return (IG.RankedStrategies [optIG], OptimizationResult eqtyBrm trds twrs rsks)

    

data OptimizationResult = OptimizationResult {
  eqtyBroom :: Broom.Broom (Signal.Signal B.BarNo Eqty.Equity)
  , tradeList :: Trade.TradeList UTCTime P.Price
  , twr :: [(F.Fraction, Dist.CDF TWR.TWR)]
  , risk :: [(F.Fraction, Dist.CDF Risk.Risk)]
  }

instance TR.ToReport (TR.OptimizationData (OptimizationInput UTCTime P.Price) OptimizationResult) where
  toReport (TR.OptimizationData optInp (OptimizationResult brm trdList twrs rsks)) = do
    let nOfSamp = 20

        showFrac :: F.Fraction -> String
        showFrac (F.Fraction fr) = printf "Frac %.02f" fr


        g mdf cmp (F.Fraction fr, Dist.CDF vs) =
          [printf "%.02f" fr, maybe "n/a" (printf "%.02f%%" . (100*) . mdf . fst) (Vec.find (cmp . snd) vs)]
        twrTable10 = ["Fraction f", "P(TWR <= 1)"] : map (g id (not . (<= 1.0))) twrs
        twrTable12 = ["Fraction f", "P(TWR > 1.2)"] : map (g (1-) (> 1.2)) twrs
    
        h (F.Fraction fr, Dist.CDF vs) =
          [printf "%.02f" fr, maybe "n/a" (printf "%.02f%%" . (100*) . (1-) . fst)  (Vec.find ((>0.2) . snd) vs)]
        riskTable = ["Fraction f", "P(max. drawdown > 20%)"] : map h rsks

        signal = Vec.map (fmap P.unPrice) (Signal.unSignal (optSample optInp))

    Rep.text "Buying/Selling at the crossing of two moving averages, window sizes 11 and 19."
    
    Rep.subheader "Optimization Input"
    Rep.chart (Style.axTitle "Symbol") (Style.axTitle "Price", [Line.line "Price" (optSample optInp)])

    Rep.subsubheader "Sample Statistics"
    SStat.stats2para (SStat.sampleStatistics signal)

    Rep.subheader "Optimization Result"

    Rep.subsubheader "Trade Statistics"

    Rep.divs $ map TStat.stats2para (TStat.tradeStatistics id trdList)


    Rep.subsubheader "Generated Broom"
    Rep.text ("Number of Monte Carlo samples: " ++ show (mcN optInp) ++ ", showing " ++ show nOfSamp)
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Equity", Broom.broom2chart nOfSamp brm)

    Rep.subsubheader "Terminal wealth relative"
    Rep.chart (Style.axTitle "Percent") (Style.axTitle "TWR", map (\(fr, cdf) -> Line.line (showFrac fr) cdf) twrs)
    Rep.text ("The probability that terminal wealth relative is less than factor 1.0, respectivly greater than 1.2, at fraction f:")
    Rep.horizontal $ do
      Rep.floatLeft $ Rep.htable twrTable10
      Rep.floatLeft $ Rep.htable twrTable12

    Rep.subsubheader "Risk of Drawdown"
    Rep.chart (Style.axTitle "Percent") (Style.axTitle "Drawdown", map (\(fr, cdf) -> Line.line (showFrac fr) cdf) rsks)
    Rep.text ("Risk of max. drawdown greater than 20% at fraction f:")
    Rep.htable riskTable



--------------------------------------------------------

data BacktestInput t ohlc = BacktestInput {
  initialEquity :: Eqty.Equity
  , pricesInput :: Signal.Signal t ohlc
  }
    
instance (Ord t, B.Time t, Num (B.DeltaT t)) => BT.Backtest (BacktestInput t P.Price) where
  type BacktestReportTy (BacktestInput t P.Price) = BacktestResult t

  backtest (IG.NonEmptyList (IG.OptimizedImpulseGenerator optStrat) _) (BacktestInput initEqty ps) =
    let impSig = optStrat ps
        es = BT.equitySignal id SF.stepFuncNoCommissionFullFraction initEqty impSig ps
    in BacktestResult impSig es

data BacktestResult t = BacktestResult {
  impulses :: IS.ImpulseSignal t
  , eqties :: ES.EquitySignal t
  }

instance (E.PlotValue t, Show t) =>
         TR.ToReport (TR.BacktestData (BacktestInput t P.Price) (BacktestResult t)) where
  
  toReport (TR.BacktestData (BacktestInput inEq ps) (BacktestResult impSig es)) = do
    let left = (Style.axTitle "Equity", [Line.line "Price" ps, Line.line "Backtest" es])
        right = (Style.impulseAxisConf, [Line.line "down buy / up sell" (IS.curve ps impSig)])

    Rep.subheader "Backtest Result"
    Rep.text "Trading at full fraction, no commissions"

    Rep.chartLR (Style.axTitle "Time") left right
    Rep.text ("Initial Equity: " ++ show inEq)

    case Signal.length es > 0 of
      True -> do
        Rep.text ("Starting with equity " ++ show (Signal.head es))
        Rep.text ("Ending with equity " ++ show (Signal.last es))
      False -> do
        Rep.text "No trades occured"


--------------------------------------------------------

instance OD.OHLCData (OptimizationInput t ohlc) where
  type OHLCDataTy (OptimizationInput t ohlc) = ohlc

instance OD.OHLCData (BacktestInput t ohlc) where
  type OHLCDataTy (BacktestInput t ohlc) = ohlc

--------------------------------------------------------

example :: IO ()
example = do

  let mu = Black.Mu 0.1
      sigma = Black.Sigma 0.2
      start = Eqty.Equity 100
      seed = 49

  sample <- Black.blackScholesDet seed (T.yearsN 4) start mu sigma
  
  -- let sample = Signal.Signal (Vec.map (fmap f) samp)
  -- let sample = Signal.Signal (Vec.map (fmap f) TD.test2)

  let analysis :: Ana.Analysis (OptimizationInput UTCTime P.Price) (BacktestInput UTCTime P.Price)
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (IG.impulsesFromTwoMovingAverages P.unPrice (IG.WindowSize 11) (IG.WindowSize 19))
        , Ana.optimizationInput = OptimizationInput {
            optSample = sample
            , mcN = 1000
            , optInitialEquity = Eqty.Equity 1000
            , forcastHorizon = B.Bars 1000
            , stepFunc = SF.stepFuncNoCommission -- stepFuncNoCommissionFullFraction
            , fractions = map F.Fraction [0.1, 0.5, 1, 1.5, 2.0, 5.0] -- [0.1, 0.2 .. 2]
            }
        , Ana.backtestInput = BacktestInput (Eqty.Equity 102.5) sample
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t

