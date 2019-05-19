{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Example.ReplayingTrades where

import Control.Applicative (liftA2)

import Data.Time.Clock (UTCTime(..), NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import Text.Printf (printf)

import Trade.Type.Bars (DeltaTy, Add, BarLength(..), BarNo(..))
import Trade.Type.Broom (Broom, broom2chart)
import Trade.Type.Delta (ToDelta)
-- import qualified Trade.Type.Distribution as Dist
import Trade.Type.Step.Commission (Commission(..), noCommission)
import Trade.Type.Step.Fraction (Fraction(..), fullFraction)
import Trade.Type.Step.Interests (Interests(..), interests)
import Trade.Type.Step.Algorithm (StepFunction)
import Trade.Type.Equity (Equity(..))

-- import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), OptimizedImpulseGenerator(..), RankedStrategies(..), impulsesFromTwoMovingAverages, invert)

import Trade.Type.ImpulseGenerator (ImpulseGenerator(..), RankedStrategies(..))
import qualified Trade.Type.ImpulseGenerator as IG

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
import Trade.Type.Conversion.Invest2Impulse (Invest2Impulse, invest2impulse)
import Trade.Type.Yield (ToYield)

-- import Trade.Algorithm.MovingAverage (WindowSize(..))
import Trade.Strategy.Type (Window(..))
import Trade.Strategy.Library.MovingAverages (movingAverages)

import Trade.TStatistics.Statistics (DeltaTyStats)

import qualified Trade.Analysis.TWR as TWR
import qualified Trade.Analysis.Risk as Risk
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.ToReport as TR
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT


import qualified Trade.Report.Line as Line
import qualified Trade.Report.Heatmap as Heat
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import qualified Trade.Report.Table as Tab
import Trade.Report.Pretty (Pretty)

import Trade.MonteCarlo.ResampleTrades.Broom (MCConfig(..), mc, MCCount(..))
import Trade.MonteCarlo.Simulation.BlackScholes (Mu(..), Sigma(..), blackScholesDet)

import qualified Trade.Test.Time as T

import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Database as Bin
import qualified Trade.Timeseries.Binance.Interval as Bin
import qualified Trade.Timeseries.Binance.Symbol as Bin
import qualified Trade.Timeseries.Url as Url

data Symbol = ASym deriving (Show, Eq, Ord)

{-
data OptimizationInput stgy sym t ohlc = OptimizationInput {
  optSample :: (sym, Signal t ohlc)
  , igInput :: [(Window, Window)]
  , mcConfig :: MCConfig t
  , optEquity :: Equity
  , step :: StepTy stgy t
  }

  
data OptimizationResult t sym stgy = OptimizationResult {
  result :: Experiment.Result stgy sym t Price
  , broom :: Broom (Signal t Equity)
  , lastEquities :: Map (Window, Window) Equity
  }
  
instance ( Ord sym
         , Show sym, Show t
         , Ord t
         , Add t
         , Invest2Impulse stgy
         , TradeList2DeltaTradeList stgy
         , Impulse2TradeList stgy
         , StepFunction (StepTy stgy) t) =>
  Opt.Optimize (OptimizationInput stgy sym t Price) where

  type OptReportTy (OptimizationInput stgy sym t Price) = OptimizationResult t sym stgy
  type OptInpTy (OptimizationInput stgy sym t Price) = (Window, Window)

  optimize (ImpulseGenerator strat) optInp = do
    let findBestWinSize winSize acc =
          let e = Experiment.Input (step optInp) (optEquity optInp) (strat winSize) [optSample optInp]
          in Map.insert winSize (Experiment.conduct e) acc

        p e0 e1 = compare (Experiment.lastEquity e1) (Experiment.lastEquity e0)

        strats = List.foldr findBestWinSize Map.empty (igInput optInp)

        f (Experiment.Result inp _) = Experiment.impulseGenerator inp
        sortedStarts@(optStrat:_) = map f (List.sortBy p (Map.elems strats))
 
        expmnt = Experiment.Input (step optInp) (optEquity optInp) optStrat [optSample optInp]
        res = Experiment.conduct expmnt
    
    brm <- mc res (mcConfig optInp)

    return (RankedStrategies sortedStarts, OptimizationResult res brm (fmap Experiment.lastEquity strats))


instance ( Show sym
         , E.PlotValue t
         , Add t
         , Num (DeltaTy t)
         , Real (DeltaTy t)
         , Line.XTy (Vec.Vector (t, Equity)) ~ t
         , Line.YTy (Vec.Vector (t, Equity)) ~ Equity
         , Line.ToLine (Vec.Vector (t, Equity))
         , Pretty t
         , Pretty (DeltaTy t)
         , Pretty (DeltaTyStats t)
         , StepFunction (StepTy stgy) t) =>
  TR.ToReport (TR.OptimizationData (OptimizationInput stgy sym t Price) (OptimizationResult t sym stgy)) where

  toReport (TR.OptimizationData optInp (OptimizationResult res brm lastEqty)) = do
    let nOfSamp = 20
    
    Rep.subheader "Optimization Input"

    Experiment.render "Symbol at Close" "Backtest" res

    Rep.subsubheader "Table of last equities"

    Heat.heatmap
      (unEquity (optEquity optInp))
      (Map.mapKeys (\(Window y, Window x) -> (y, x)) (fmap unEquity lastEqty))

    Rep.subsubheader "Generated Broom"
    Rep.text ("Showing " ++ show nOfSamp ++ " Monte Carlo samples")
    Rep.chart
      (Style.axTitle "Time" "Bars" {- :: Style.AxisConfig t Price -})
      (Style.axTitle "Time" "Equity" {- :: Style.AxisConfig Equity t -}, broom2chart nOfSamp brm)


data BacktestInput stgy sym t ohlc = BacktestInput {
  btEquity :: Equity
  , btSample :: (sym, Signal t ohlc)
  , btStep :: StepTy stgy t
  }
  
data BacktestResult stgy sym t ohlc = BacktestResult (Experiment.Result stgy sym t ohlc)
                          
instance ( ToDelta ohlc
         , Ord sym
         , Ord t
         , Add t
         , TradeList2DeltaTradeList stgy
         , Impulse2TradeList stgy
         , Invest2Impulse stgy
         , StepFunction (StepTy stgy) t) => BT.Backtest (BacktestInput stgy sym t ohlc) where
  
  type BacktestReportTy (BacktestInput stgy sym t ohlc) = BacktestResult stgy sym t ohlc

  backtest (NonEmptyList optStrat _) (BacktestInput initEqty ps step) =
    let expmnt = Experiment.Input step initEqty optStrat [ps]
    in BacktestResult (Experiment.conduct expmnt)

instance (E.PlotValue t
         , Show sym
         , Add t
         , Real (DeltaTy t)
         , E.PlotValue ohlc
         , Line.ToLine (Vec.Vector (t, ohlc))
         , Line.XTy (Vec.Vector (t, ohlc)) ~ t
         , Line.YTy (Vec.Vector (t, ohlc)) ~ ohlc
         , StepFunction (StepTy stgy) t
         , Pretty t, Pretty (DeltaTy t)
         , Pretty (DeltaTyStats t)
         , Pretty ohlc
         , Eq ohlc
         , ToYield ohlc) =>
  TR.ToReport (TR.BacktestData (BacktestInput stgy sym t ohlc) (BacktestResult stgy sym t ohlc)) where
  
  toReport (TR.BacktestData _ (BacktestResult res)) = do
    Rep.subheader "Backtest"
    Experiment.render "Symbol at Close" "Backtest" res
 
 --------------------------------------------------------

instance OD.OHLCData (OptimizationInput stgy sym t ohlc) where
  type OHLCDataTy (OptimizationInput stgy sym t ohlc) = ohlc


instance OD.OHLCData (BacktestInput stgy sym t ohlc) where
  type OHLCDataTy (BacktestInput stgy sym t ohlc) = ohlc

--------------------------------------------------------

barLength :: BarLength
barLength = Hour 1


getSymbol :: Bin.Symbol -> IO (UTCTime, Signal UTCTime Price)
getSymbol sym = do

  now <- getCurrentTime
  
  let req = Bin.RequestParams {
        Bin.baseUrl = Bin.binanceBaseUrl
        , Bin.symbol = sym
        , Bin.interval = Bin.Interval barLength
        , Bin.limit = Just 1000
        , Bin.from = Nothing
        , Bin.to = Just now -- ((fromIntegral (negate (10*24*60*60))) `addUTCTime` now)
        }

      toSignal row = (Bin.toDate row, Price (unClose (Bin.close row)))

      mcBegin = UTCTime (fromGregorian 2020 1 1) 0
  
  fmap ((\x -> (mcBegin, x)) . Signal . Vec.map toSignal) (Bin.getTicker req)


blackScholes :: IO (BarNo, Signal BarNo Price)
blackScholes = do
  
  let mu = Mu 0.5
      sigma = Sigma 0.5
      start = Price 1000
      seed = 49
      vs = Vec.fromList (map BarNo [0 .. 1000])

      mcBegin = BarNo 0
      
--  blackScholesDet seed (T.yearsN 4) start mu sigma
  fmap (\x -> (mcBegin, x)) (blackScholesDet seed vs start mu sigma)

  

example :: IO ()
example = do

  let sym = Bin.USDT Bin.BTCUSDT
  (mcBegin, sample) <- getSymbol (Bin.USDT Bin.BTCUSDT)
  -- (mcBegin, sample) <- blackScholes


  let Signal.Sample inSamp outOfSamp = Signal.split 0.75 sample
  
      f (j, k) = (Window j, Window k)
      wins = map f (filter (uncurry (/=)) (liftA2 (,) [1 .. 100] [1 .. 100]))
      -- wins = map f (filter (uncurry (/=)) (liftA2 (,) [5..10] [1..10]))

      longStep = LongStep {
        longFraction = Fraction 1
        , longCommission = Commission (const 0) -- (\c -> 0.001*c)
        }



      -- rtf :: NominalDiffTime -> Double
      rtf dt =
        let day = 24*60*60
        in realToFrac dt / day
  
      shortStep :: StepTy Short UTCTime
      shortStep = ShortStep {
        shortFraction = Fraction 1
        , shortCommission = Commission (const 0)
        , shortInterests = Interests (interests rtf 0.0)
        }

  let analysis :: Ana.Analysis (OptimizationInput Long Bin.Symbol UTCTime Price) (BacktestInput Long Bin.Symbol UTCTime Price)
      analysis = Ana.Analysis {
        Ana.title = "Replaying Long Trades"
        , Ana.impulseGenerator = IG.ImpulseGenerator (\(j, k) -> IG.OptimizedImpulseGenerator (movingAverages j k))

        , Ana.optimizationInput = OptimizationInput {
            optSample = (sym, inSamp)
            , igInput = wins
            , optEquity = Equity (unPrice (snd (Signal.head inSamp)))
            , mcConfig = MCConfig {
                mcBars = 60
                , mcCount = MCCount 10
                , mcBegin = mcBegin
                }
            , step = longStep
            -- , step = shortStep
            }
        , Ana.backtestInput = BacktestInput {
            btEquity = Equity (unPrice (snd (Signal.head outOfSamp)))
            , btSample = (sym, outOfSamp)
            , btStep = longStep
            }
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
 
-}
