{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Example.Optimize where

import Control.Monad.Trans (liftIO)

import Control.Monad (replicateM)

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
import qualified Trade.Type.History as Hist
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Broom as Broom
import qualified Trade.Type.Distribution as Dist
import qualified Trade.Type.Fraction as F
import qualified Trade.Type.Trade as Trade

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Price as PS
import qualified Trade.Type.Signal.Impulse as IS
import qualified Trade.Type.Signal.Equity as ES


import qualified Trade.Type.Conversion.Impulse2Trade as I2T
import qualified Trade.Type.Conversion.Trade2NormTrade as T2NT

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

import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC
import qualified Trade.MonteCarlo.ResampleTrades.OffsettedNormTradeList as ONTL
import qualified Trade.MonteCarlo.ResampleTrades.Broom as RTBroom


import qualified Trade.MonteCarlo.Simulation.BlackScholes as Black

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Curve as Curve

import qualified Trade.Test.Time as T
import qualified Trade.Test.Data as TD

import qualified Trade.Report.Style as Style


import Debug.Trace

--------------------------------------------------------

generateImpulseSignal ::
  (OHLC.OHLCInterface ohlc) =>
  Int -> Int -> PS.PriceSignal ohlc -> IS.ImpulseSignal
generateImpulseSignal s b (Signal.Signal ps) =
  let f (t, x) = O.unClose (OHLC.ohlcClose x)
      qs = Vec.toList (Vec.map f ps)

      sell xs | length (take s xs) < s = map (const Nothing) xs 
      sell xs@(_:as) =
        let (y:ys, zs) = splitAt s xs
        in case all (<y) ys of
             True -> map (const Nothing) ys ++ [Just Imp.Sell] ++ buy zs
             False -> Nothing : sell as

      buy xs | length (take b xs) < b = map (const Nothing) xs
      buy xs@(_:as) =
        let (y:ys, zs) = splitAt b xs
        in case all (>y) ys of
             True -> map (const Nothing) ys ++ [Just Imp.Buy] ++ sell zs
             False -> Nothing : buy as
             
      g i x = (fst (ps Vec.! i), x)
      res = Vec.imap g (Vec.fromList (buy qs))

  in Signal.Signal res


data OptimizationInput ohlc = OptimizationInput {
  optSample :: PS.PriceSignal ohlc
  , optTradeAt :: ohlc -> O.Close
  , mcN :: Int
  , optInitialEquity :: Eqty.Equity
  , forcastHorizon :: B.Bars
  , stepFunc :: F.Fraction -> SF.StepFunc
  , fractions :: [F.Fraction]
  }
  

instance Opt.Optimize OptimizationInput where
  type OptReportTy OptimizationInput = OptimizationResult
  
  optimize strat optInp = do
    let optStrat = strat optInp
        ts = I2T.impulse2trade (optSample optInp) (optStrat (optSample optInp))
        nts = T2NT.trade2normTrade (fmap (optTradeAt optInp) ts)
    yieldHistBroom <- RTBroom.normHistoryBroom (forcastHorizon optInp) (mcN optInp) nts

    let sf = stepFunc optInp
        eq = optInitialEquity optInp

        eqtyBroom = Broom.yield2equity (sf (F.Fraction 1)) eq yieldHistBroom
    
        f fr (ts, rs) =
          let eb = Broom.yield2equity (sf fr) eq yieldHistBroom
              twr = TWR.terminalWealthRelative eq eb
              risk = Risk.risk eb
          in ((fr, twr):ts, (fr, risk):rs)

    
        (twrs, rsks) = List.foldr f ([], []) (fractions optInp)

        tl = fmap (optTradeAt optInp) ts
    
    return (optStrat, OptimizationResult eqtyBroom tl twrs rsks)

    

data OptimizationResult = OptimizationResult {
  eqtyBroom :: Broom.Broom (Hist.History Eqty.Equity)
  , tradeList :: Trade.TradeList O.Close
  , twr :: [(F.Fraction, Dist.CDF TWR.TWR)]
  , risk :: [(F.Fraction, Dist.CDF Risk.Risk)]
  }

instance (OHLC.OHLCInterface ohlc) =>
         TR.ToReport (TR.OptimizationData ohlc OptimizationInput OptimizationResult) where
  toReport (TR.OptimizationData optInp (OptimizationResult brm trdList twrs rsks)) = do
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

    Rep.subheader "Optimization Input"
    Rep.candle "Symbol" [toCandle (optSample optInp)]


    Rep.subsubheader "Sample Statistics"
    SStat.stats2para (SStat.sampleStatistics signal)

    Rep.subheader "Optimization Result"

    Rep.subsubheader "Trade Statistics"

    Rep.divs $ map TStat.stats2para (TStat.tradeStatistics id trdList)


    Rep.subsubheader "Generated Broom"
    Rep.text ("Number of Monte Carlo samples: " ++ show (mcN optInp) ++ ", showing " ++ show nOfSamp)
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Equity", Broom.broom2chart nOfSamp brm)

    Rep.subsubheader "Terminal wealth relative"
    Rep.chart (Style.axTitle "Percent") (Style.axTitle "TWR", map (\(fr, cdf) -> Rep.line (showFrac fr) cdf) twrs)
    Rep.text ("The probability that terminal wealth relative is less than factor 1.0, respectivly greater than 1.2, at fraction f:")
    Rep.horizontal $ do
      Rep.floatLeft $ Rep.htable twrTable10
      Rep.floatLeft $ Rep.htable twrTable12

    Rep.subsubheader "Risk of Drawdown"
    Rep.chart (Style.axTitle "Percent") (Style.axTitle "Drawdown", map (\(fr, cdf) -> Rep.line (showFrac fr) cdf) rsks)
    Rep.text ("Risk of max. drawdown greater than 20% at fraction f:")
    Rep.htable riskTable



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
    let bts = Vec.map (fmap Eqty.unEquity) (Signal.unSignal es)
        ps' = Vec.map (fmap (O.unOHLC . trdAt)) (Signal.unSignal ps)
        left = (Style.axTitle "Equity", [Rep.line "Symbol at Close" ps', Rep.line "Backtest" bts])
        right = (Style.impulseAxisConf, [Rep.line "down buy / up sell" (Curve.curve impSig)])

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

  let mu = Black.Mu 0.2
      sigma = Black.Sigma 0.15
      start = Eqty.Equity 100
      seed = 53

  -- samp <- Black.blackScholesDet seed (T.yearsN 1) start mu sigma
  
  let sample = Signal.Signal (Vec.map (fmap f) TD.test2)
  -- let sample = Signal.Signal (Vec.map (fmap f) samp)

  let trdAt = OHLC.ohlcClose
  
      analysis :: Ana.Analysis OHLC.OHLC OptimizationInput BacktestInput
      analysis = Ana.Analysis {
        Ana.title = "An Example Report"
        , Ana.impulseGenerator = IG.optImpGen2impGen (generateImpulseSignal 3 2)
        , Ana.optimizationInput = OptimizationInput {
            optSample = sample
            , optTradeAt = trdAt
            , mcN = 1000
            , optInitialEquity = Eqty.Equity 1000
            , forcastHorizon = B.Bars 1000
            , stepFunc = SF.stepFuncNoCommission -- stepFuncNoCommissionFullFraction
            , fractions = map F.Fraction [0.1, 0.5, 1, 1.5, 2.0, 5.0] -- [0.1, 0.2 .. 2]
            }
        , Ana.backtestInput = BacktestInput trdAt (Eqty.Equity 10) sample
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
