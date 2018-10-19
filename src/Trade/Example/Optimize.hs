{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Example.Optimize where

import Control.Monad.Trans (liftIO)

import Control.Monad (replicateM)

import Data.Time.Clock (UTCTime)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import Text.Printf (printf)

import qualified Text.Blaze.Html5 as H5

import qualified Trade.Type.Impulse as Imp
import qualified Trade.Type.Equity as Eqty
import qualified Trade.Type.OHLC as O
import qualified Trade.Type.Bars as B
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Broom as Broom
import qualified Trade.Type.Distribution as Dist
import qualified Trade.Type.Fraction as F
import qualified Trade.Type.Trade as Trade
import qualified Trade.Type.OffsettedNormTradeList as ONTL
import qualified Trade.Type.Yield as Y

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.Signal.Impulse as IS
import qualified Trade.Type.Signal.Equity as ES


import qualified Trade.Type.Conversion.Impulse2Trade as I2T
import qualified Trade.Type.Conversion.Trade2NormTrade as T2NT
import qualified Trade.Type.Conversion.Equity2Yield as E2Y

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

import qualified Trade.TStatistics.SampleStatistics as SStat
import qualified Trade.TStatistics.TradeStatistics as TStat

import qualified Trade.MonteCarlo.ResampleTrades.Broom as RTBroom


import qualified Trade.MonteCarlo.Simulation.BlackScholes as Black

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Curve as Curve
import qualified Trade.Report.Line as Line

import qualified Trade.Test.Time as T
import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


import Debug.Trace

--------------------------------------------------------

{-

data OptimizationInput ohlc = OptimizationInput {
  optSample :: PS.PriceSignal ohlc
  , optTradeAt :: ohlc -> O.Close
  , mcN :: Int
  , optInitialEquity :: Eqty.Equity
  , forcastHorizon :: B.Bars
  , stepFunc :: F.Fraction -> SF.StepFunc Y.Yield
  , fractions :: [F.Fraction]
  }
  

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  
  optimize strat optInp = do
    let optStrat = strat optInp
    
        sample = fmap (Eqty.Equity . O.unOHLC . (optTradeAt optInp)) (optSample optInp)
        len = Signal.length (optSample optInp)

        yields :: Signal.Signal UTCTime Y.LogYield
        yields = E2Y.equity2yield sample
        sampleStats = SStat.sampleStatistics yields

        mu = Black.Mu (fromIntegral len * SStat.mean sampleStats)
        sigma = Black.Sigma (sqrt (fromIntegral len) * SStat.stdDev sampleStats)

    brm <- Black.priceSignalBroom (forcastHorizon optInp) (mcN optInp) (optInitialEquity optInp) mu sigma

    let eqtyBrm@(Broom.Broom (x:_)) = fmap (fmap Eqty.Equity) brm

        t = optStrat x

    return (optStrat, OptimizationResult eqtyBrm mu sigma sampleStats)

    

data OptimizationResult = OptimizationResult {
  broom :: Broom.Broom (Signal.Signal B.BarNo Eqty.Equity)
  , muOR :: Black.Mu
  , sigmaOR :: Black.Sigma
  , sampleStats :: SStat.SampleStatistics UTCTime
  }

instance (OHLC.OHLCInterface ohlc) =>
         TR.ToReport (TR.OptimizationData ohlc OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData optInp (OptimizationResult brm mu sigma sStats)) = do
    let toC (t, ohlc) =
          let c = E.Candle t
                  (O.unOHLC $ OHLC.ohlcLow ohlc)
                  (O.unOHLC $ OHLC.ohlcOpen ohlc)
                  0
                  (O.unOHLC $ OHLC.ohlcClose ohlc)
                  (O.unOHLC $ OHLC.ohlcHigh ohlc)
          in c
        toCandle (Signal.Signal cs) = Vec.map toC cs

        nOfSamp = 20
        
{-

        showFrac :: F.Fraction -> String
        showFrac (F.Fraction fr) = printf "Frac %.02f" fr


        g mdf cmp (F.Fraction fr, Dist.CDF vs) =
          [printf "%.02f" fr, maybe "n/a" (printf "%.02f%%" . (100*) . mdf . fst)  (Vec.find (cmp . snd) vs)]
        twrTable10 = ["Fraction f", "P(TWR <= 1)"] : map (g id (not . (<= 1.0))) twrs
        twrTable12 = ["Fraction f", "P(TWR > 1.2)"] : map (g (1-) (> 1.2)) twrs
    
        h (F.Fraction fr, Dist.CDF vs) =
          [printf "%.02f" fr, maybe "n/a" (printf "%.02f%%" . (100*) . (1-) . fst)  (Vec.find ((>0.2) . snd) vs)]
        riskTable = ["Fraction f", "P(max. drawdown > 20%)"] : map h rsks

        signal = Vec.map (fmap (O.unClose . optTradeAt optInp)) (Signal.unSignal (optSample optInp))
-}
    
    Rep.subheader "Optimization Input"
    Rep.candle "Symbol" [toCandle (optSample optInp)]

    Rep.subsubheader "Sample Statistics of Log Yields"
    SStat.stats2para sStats
    
    Rep.subsubheader "Generated Broom"
    Rep.text ("Black-Scholes with parameters: " ++ show mu ++ ", " ++ show sigma)
    Rep.text ("Number of Monte Carlo samples: " ++ show (mcN optInp) ++ ", showing " ++ show nOfSamp)
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Equity", Broom.broom2chart nOfSamp brm)


    
{-
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
-}



--------------------------------------------------------

data BacktestInput ohlc = BacktestInput {
  tradeAt :: ohlc -> O.Close
  , initialEquity :: Eqty.Equity
  , pricesInput :: PS.PriceSignal ohlc
  }
    
instance BT.Backtest BacktestInput where
  type BacktestReportTy BacktestInput = BacktestResult

  backtest optStrat (BacktestInput trdAt initEqty ps) =
    let impSig = optStrat ps
        es = BT.equitySignal trdAt initEqty impSig ps
    in BacktestResult impSig es

data BacktestResult = BacktestResult {
  impulses :: IS.ImpulseSignal
  , eqties :: ES.EquitySignal
  }

instance TR.ToReport (TR.BacktestData ohlc BacktestInput BacktestResult) where
  toReport (TR.BacktestData (BacktestInput trdAt inEq ps) (BacktestResult impSig es)) = do
    let Signal.Signal bts = fmap Eqty.unEquity es
        ps' = fmap (O.unOHLC . trdAt) ps
        left = (Style.axTitle "Equity", [Line.line "Symbol at Close" ps', Line.line "Backtest" bts])
        right = (Style.impulseAxisConf, [Line.line "down buy / up sell" (Curve.curve impSig)])

    Rep.subheader "Backtest Result"


    Rep.chartLR (Style.axTitle "Time") left right
    Rep.text ("Initial Equity: " ++ show inEq)

    case Vec.length bts > 0 of
      True -> do
        Rep.text ("Starting with equity " ++ show (Vec.head bts))
        Rep.text ("Ending with equity " ++ show (Vec.last bts))
      False -> do
        Rep.text "No trades occured"

--------------------------------------------------------

example :: IO ()
example = do

  let f x = OHLC.OHLC (O.Open (x+0.5)) (O.High (x+1)) (O.Low (x-1)) (O.Close x) (O.Volume 1000)
  
  let mu = Black.Mu 0.4
      sigma = Black.Sigma 0.2
      start = Eqty.Equity 100
      seed = 41

  samp <- Black.blackScholesDet seed (T.yearsN 4) start mu sigma
  
  -- let sample = Signal.Signal (Vec.map (fmap f) TD.test2)
  let Signal.Sample inSample outOfSample = Signal.split 0.75 (Signal.Signal (Vec.map (fmap f) samp))

  let trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OHLC.OHLC OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (IG.impulsesFromMovingAverages 17 7)
        , Ana.optimizationInput = OptimizationInput {
            optSample = inSample
            , optTradeAt = trdAt
            , mcN = 1000
            , optInitialEquity = Eqty.Equity 1000
            , forcastHorizon = B.Bars 1000
            , stepFunc = SF.stepFuncNoCommission -- stepFuncNoCommissionFullFraction
            , fractions = map F.Fraction [0.1, 0.5, 1, 1.5, 2.0, 5.0]
            }
        , Ana.backtestInput = BacktestInput trdAt (Eqty.Equity 100) outOfSample
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t


-}
