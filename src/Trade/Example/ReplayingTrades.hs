{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Example.ReplayingTrades where

import Control.Applicative (liftA2)

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import Text.Printf (printf)

import Trade.Type.Bars (BarNo, DeltaTy(NDT))
import Trade.Type.Broom (Broom, broom2chart)
-- import qualified Trade.Type.Distribution as Dist
import Trade.Type.Step.Commission (Commission(..), noCommission)
import Trade.Type.Step.Fraction (Fraction(..), fullFraction)
import Trade.Type.Step.Interests (Interests(..), interests)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), OptimizedImpulseGenerator(..), RankedStrategies(..), impulsesFromTwoMovingAverages, invert)
import Trade.Type.NonEmptyList (NonEmptyList(..))
import Trade.Type.OHLC (Close(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.Step (StepTy(LongStep, ShortStep), longFraction, shortFraction, longCommission, shortCommission, shortInterests)
import Trade.Type.Strategy (Long, Short)

import qualified Trade.Type.Experiment as Experiment
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList)
import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList)

import Trade.Algorithm.MovingAverage (WindowSize(..))


import qualified Trade.Analysis.TWR as TWR
import qualified Trade.Analysis.Risk as Risk
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT

--import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import qualified Trade.Report.Table as Tab


import Trade.MonteCarlo.ResampleTrades.Broom (MCConfig(..), mc, MCCount(..))
import Trade.MonteCarlo.Simulation.BlackScholes (Mu(..), Sigma(..), blackScholesDet)

import qualified Trade.Test.Time as T

import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Database as Bin
import qualified Trade.Timeseries.Binance.Interval as Bin
import qualified Trade.Timeseries.Binance.Symbol as Bin
import qualified Trade.Timeseries.Url as Url

data OptimizationInput stgy t ohlc = OptimizationInput {
  optSample :: Signal t ohlc
  , igInput :: [(WindowSize, WindowSize)]
  , mcConfig :: MCConfig t
  , optEquity :: Equity
  , step :: StepTy stgy t
  }

  
data OptimizationResult stgy = OptimizationResult {
  result :: Experiment.Result stgy UTCTime Price
  , broom :: Broom (Signal UTCTime Equity)
  , lastEquities :: Map (WindowSize, WindowSize) Equity
  }
  
instance (TradeList2DeltaTradeList stgy, Impulse2TradeList stgy, StepFunction (StepTy stgy) UTCTime) =>
  Opt.Optimize (OptimizationInput stgy UTCTime Price) where

  type OptReportTy (OptimizationInput stgy UTCTime Price) = OptimizationResult stgy
  type OptInpTy (OptimizationInput stgy UTCTime Price) = (WindowSize, WindowSize)

  optimize (ImpulseGenerator strat) optInp = do
    let findBestWinSize winSize acc =
          let e = Experiment.Input (step optInp) (optEquity optInp) (strat winSize) (optSample optInp)
          in Map.insert winSize (Experiment.conduct e) acc

        p e0 e1 = compare (Experiment.lastEquity e1) (Experiment.lastEquity e0)

        strats = List.foldr findBestWinSize Map.empty (igInput optInp)

        f (Experiment.Result inp _) = Experiment.impulseGenerator inp
        sortedStarts@(optStrat:_) = map f (List.sortBy p (Map.elems strats))
 
        expmnt = Experiment.Input (step optInp) (optEquity optInp) optStrat (optSample optInp)
        res = Experiment.conduct expmnt

    brm <- mc res (mcConfig optInp)

    return (RankedStrategies sortedStarts, OptimizationResult res brm (fmap Experiment.lastEquity strats))

instance TR.ToReport (TR.OptimizationData (OptimizationInput stgy UTCTime Price) (OptimizationResult stgy)) where
  toReport (TR.OptimizationData optInp (OptimizationResult res brm lastEqty)) = do
    let nOfSamp = 20
    
    Rep.subheader "Optimization Input"

    Experiment.render "Symbol at Close" "Backtest" res

    Rep.subsubheader "Table of last equities"

    Tab.table
      (unEquity (optEquity optInp))
      (Map.mapKeys (\(WindowSize y, WindowSize x) -> (y, x)) (fmap unEquity lastEqty))

    Rep.subsubheader "Generated Broom"
    Rep.text ("Showing " ++ show nOfSamp ++ " Monte Carlo samples")
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Equity", broom2chart nOfSamp brm)


data BacktestInput t ohlc = BacktestInput {
  backtestEquity :: Equity
  , pricesInput :: Signal t ohlc
  }
  
data BacktestResult = BacktestResult
                          
instance BT.Backtest (BacktestInput t Price) where
  type BacktestReportTy (BacktestInput t Price) = BacktestResult

  backtest (NonEmptyList (OptimizedImpulseGenerator optStrat) _) (BacktestInput initEqty ps) =
    BacktestResult

instance TR.ToReport (TR.BacktestData (BacktestInput t Price) BacktestResult) where
  
  toReport (TR.BacktestData (BacktestInput inEq ps) BacktestResult) = do
    Rep.text "no backtest"
 
 --------------------------------------------------------

instance OD.OHLCData (OptimizationInput stgy t ohlc) where
  type OHLCDataTy (OptimizationInput stgy t ohlc) = ohlc


instance OD.OHLCData (BacktestInput t ohlc) where
  type OHLCDataTy (BacktestInput t ohlc) = ohlc

--------------------------------------------------------

getSymbol :: Bin.Symbol -> IO (Signal UTCTime Price)
getSymbol sym = do
  
  let req = Bin.RequestParams {
        Bin.baseUrl = Bin.binanceBaseUrl
        , Bin.symbol = sym
        , Bin.interval = Bin.Min1
        , Bin.limit = Just 10000
        , Bin.from = Nothing
        , Bin.to = Nothing
        }

      toSignal row = (Bin.toDate row, Price (unClose (Bin.close row)))
  
  fmap (Signal . Vec.map toSignal) (Bin.getUnsafe req)

blackScholes :: IO (Signal UTCTime Price)
blackScholes = do
  
  let mu = Mu 0.5
      sigma = Sigma 0.5
      start = Equity 1000
      seed = 49

  blackScholesDet seed (T.yearsN 4) start mu sigma


example :: IO ()
example = do

  sample <- getSymbol Bin.BTCUSDT
  
  let f (j, k) = (WindowSize j, WindowSize k)
      wins = map f (filter (uncurry (/=)) (liftA2 (,) [0 .. 100] [0 .. 100]))
      -- wins = map f (filter (uncurry (/=)) (liftA2 (,) [16 .. 30] [8..16]))

      rtf :: DeltaTy UTCTime -> Double
      rtf dt =
        let day = 24*60*60
        in realToFrac dt / day

      longStep = LongStep {
        longFraction = Fraction 1
        , longCommission = Commission (const 0) -- (\c -> 0.05*c)
        }

      shortStep = ShortStep {
        shortFraction = Fraction 1
        , shortCommission = Commission (const 0)
        , shortInterests = Interests (interests rtf 0.0)
        }

  let -- analysis :: Ana.Analysis (OptimizationInput Short UTCTime Price) (BacktestInput UTCTime Price)
      analysis = Ana.Analysis {
        Ana.title = "Replaying Long Trades"
        , Ana.impulseGenerator = impulsesFromTwoMovingAverages unPrice
        , Ana.optimizationInput = OptimizationInput {
            optSample = sample
            , igInput = wins
            , optEquity = Equity (unPrice (snd (Signal.head sample)))
            , mcConfig = MCConfig {
                mcBars = NDT (50*60*60) -- (2*365 * 24*60*60)
                , mcCount = MCCount 1000
                , mcBegin = UTCTime (fromGregorian 2020 1 1) 0
                }
            , step = longStep
            -- , step = shortStep
            }
        , Ana.backtestInput = BacktestInput (Equity 3500) sample
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
   
{-
import Trade.Type.Bars (DeltaTy(Bars))
import Trade.Type.Step.Fraction (Fraction(..))

import qualified Trade.Type.Broom as Broom
-- import qualified Trade.Type.Fraction as F
-- import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Strategy as Strat
import qualified Trade.Type.Trade as Trade
import qualified Trade.Type.Yield as Y

import qualified Trade.Type.Signal as Signal
import qualified Trade.Type.Signal.Equity as ES
import qualified Trade.Type.ImpulseSignal as IS


import qualified Trade.Type.Conversion.Impulse2TradeList as I2TL
import qualified Trade.Type.Conversion.Trade2TradeYield as T2TY


import qualified Trade.Type.ImpulseGenerator as IG

import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Algorithm.MovingAverage as MAvg

import qualified Trade.Analysis.Backtest as BT
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.ToReport as TR

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
-}

{-

-------------------------------------------------------------------------

data OptimizationInput t ohlc = OptimizationInput {
  optSample :: Signal.Signal t ohlc
  , igInput :: (MAvg.WindowSize, MAvg.WindowSize)
  , mcN :: Int
  , optInitialEquity :: Eqty.Equity
  , forcastHorizon :: DeltaTy t
  , fractions :: [Fraction]
  }
  

instance Opt.Optimize (OptimizationInput UTCTime P.Price) where
  type OptReportTy (OptimizationInput UTCTime P.Price) = OptimizationResult
  type OptInpTy (OptimizationInput UTCTime P.Price) = (MAvg.WindowSize, MAvg.WindowSize)

  optimize (IG.ImpulseGenerator strat) optInp = do
    let optIG@(IG.OptimizedImpulseGenerator optStrat) = strat (igInput optInp)
    
        impSig = optStrat (optSample optInp)

        stp = LongStep {
          longFraction = Fraction 0.5
          , longCommission = Commission (\c -> 0.05*c)
          }

        expmnt = BT.Experiment stp initEqty impSig (optSample optInp)
        -- es = BT.equitySignal expmnt


{-
  optimize (IG.ImpulseGenerator strat) optInp = do
    let optIG@(IG.OptimizedImpulseGenerator optStrat) = strat (igInput optInp)

        trds = I2TL.impulse2tradeList Strat.Long (optSample optInp) (optStrat (optSample optInp))
        ntrds = T2TY.trade2tradeYield trds
    yieldBroom <- RTBroom.normBroom (forcastHorizon optInp) (mcN optInp) ntrds

    let sf = stepFunc optInp
        eq = optInitialEquity optInp

        eqtyBrm = Broom.yield2equity (sf (Fraction 1)) eq yieldBroom
    
        f fr (ts, rs) =
          let eb = Broom.yield2equity (sf fr) eq yieldBroom
              tw = TWR.terminalWealthRelative eq eb
              rk = Risk.risk eb
          in ((fr, tw):ts, (fr, rk):rs)
          
        (twrs, rsks) = List.foldr f ([], []) (fractions optInp)

    return (IG.RankedStrategies [optIG], OptimizationResult eqtyBrm trds twrs rsks (igInput optInp))
-}

    

data OptimizationResult = OptimizationResult {
  eqtyBroom :: Broom.Broom (Signal.Signal B.BarNo Eqty.Equity)
  , tradeList :: Trade.TradeList UTCTime P.Price
  , twr :: [(Fraction, Dist.CDF TWR.TWR)]
  , risk :: [(Fraction, Dist.CDF Risk.Risk)]
  , optWindowSize :: (MAvg.WindowSize, MAvg.WindowSize)
  }

  
{-
instance TR.ToReport (TR.OptimizationData (OptimizationInput UTCTime P.Price) OptimizationResult) where
  toReport (TR.OptimizationData optInp (OptimizationResult brm trdList twrs rsks (winK, winJ))) = do
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

        mavgK =  MAvg.mavgBar winK signal
        mavgJ =  MAvg.mavgBar winJ signal

    Rep.text ("Buying/Selling at the crossing of two moving averages, " ++ show winK ++ " and " ++ show winJ ++ ".")
    
    Rep.subheader "Optimization Input"
    Rep.chart (Style.axTitle "Symbol") (Style.axTitle "Price", [Line.line "Price" (optSample optInp), Line.line (show winK) mavgK, Line.line (show winJ) mavgJ])

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
    
instance (Ord t, B.Time t, Num (B.DeltaT t), Show t, Show (B.DeltaT t)) => BT.Backtest (BacktestInput t P.Price) where
  type BacktestReportTy (BacktestInput t P.Price) = BacktestResult t

  backtest (IG.NonEmptyList (IG.OptimizedImpulseGenerator optStrat) _) (BacktestInput initEqty ps) =
    let impSig = optStrat ps
        sf :: Steps
        sf = SF.stepFuncNoCommissionFullFraction
        expmnt = BT.Experiment Strat.Long id sf initEqty impSig ps
        es = BT.equitySignal expmnt
    in BacktestResult impSig es

data BacktestResult t = BacktestResult {
  impulses :: IS.ImpulseSignal t
  , eqties :: ES.EquitySignal t
  }

instance (E.PlotValue t, Show t) =>
         TR.ToReport (TR.BacktestData (BacktestInput t P.Price) (BacktestResult t)) where
  
  toReport (TR.BacktestData (BacktestInput inEq ps) (BacktestResult impSig es)) = do
 
    Rep.subheader "Backtest Result"
    Rep.text "Trading at full fraction, no commissions"

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
        , Ana.impulseGenerator = IG.impulsesFromTwoMovingAverages P.unPrice
        , Ana.optimizationInput = OptimizationInput {
            optSample = sample
            , igInput = (MAvg.WindowSize 12, MAvg.WindowSize 27)
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


-}
-}
