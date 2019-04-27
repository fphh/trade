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
import Trade.Type.Yield (ToYield)

import Trade.Algorithm.MovingAverage (WindowSize(..))

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

data OptimizationInput stgy t ohlc = OptimizationInput {
  optSample :: Signal t ohlc
  , igInput :: [(WindowSize, WindowSize)]
  , mcConfig :: MCConfig t
  , optEquity :: Equity
  , step :: StepTy stgy t
  }

  
data OptimizationResult t stgy = OptimizationResult {
  result :: Experiment.Result stgy t Price
  , broom :: Broom (Signal t Equity)
  , lastEquities :: Map (WindowSize, WindowSize) Equity
  }
  
instance (Ord t, Add t, TradeList2DeltaTradeList stgy, Impulse2TradeList stgy, StepFunction (StepTy stgy) t) =>
  Opt.Optimize (OptimizationInput stgy t Price) where

  type OptReportTy (OptimizationInput stgy t Price) = OptimizationResult t stgy
  type OptInpTy (OptimizationInput stgy t Price) = (WindowSize, WindowSize)

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


instance (E.PlotValue t, Add t, Num (DeltaTy t), Real (DeltaTy t), Pretty t, Pretty (DeltaTy t), Pretty (DeltaTyStats t)) =>
  TR.ToReport (TR.OptimizationData (OptimizationInput Long t Price) (OptimizationResult t Long)) where
-- instance TR.ToReport (TR.OptimizationData (OptimizationInput stgy UTCTime Price) (OptimizationResult stgy)) where
  toReport (TR.OptimizationData optInp (OptimizationResult res brm lastEqty)) = do
    let nOfSamp = 20
    
    Rep.subheader "Optimization Input"

    Experiment.render "Symbol at Close" "Backtest" res

    Rep.subsubheader "Table of last equities"

    Heat.heatmap
      (unEquity (optEquity optInp))
      (Map.mapKeys (\(WindowSize y, WindowSize x) -> (y, x)) (fmap unEquity lastEqty))

    Rep.subsubheader "Generated Broom"
    Rep.text ("Showing " ++ show nOfSamp ++ " Monte Carlo samples")
    Rep.chart (Style.axTitle "Bars") (Style.axTitle "Equity", broom2chart nOfSamp brm)


data BacktestInput stgy t ohlc = BacktestInput {
  btEquity :: Equity
  , btSample :: Signal t ohlc
  , btStep :: StepTy stgy t
  }
  
data BacktestResult stgy t ohlc = BacktestResult (Experiment.Result stgy t ohlc)
                          
instance (ToDelta ohlc, Ord t, Add t, TradeList2DeltaTradeList stgy
         , Impulse2TradeList stgy, StepFunction (StepTy stgy) t) =>
  BT.Backtest (BacktestInput stgy t ohlc) where
  
  type BacktestReportTy (BacktestInput stgy t ohlc) = BacktestResult stgy t ohlc

  backtest (NonEmptyList optStrat _) (BacktestInput initEqty ps step) =
    let expmnt = Experiment.Input step initEqty optStrat ps
    in BacktestResult (Experiment.conduct expmnt)

instance (E.PlotValue t
         , Add t, Real (DeltaTy t)
         , Line.TyX (Signal t ohlc) ~ t, Line.TyY (Signal t ohlc) ~ Double, Line.Line (Signal t ohlc)
         , StepFunction (StepTy stgy) t
         , Pretty t, Pretty (DeltaTy t), Pretty (DeltaTyStats t), Pretty ohlc
         , ToYield ohlc) =>
  TR.ToReport (TR.BacktestData (BacktestInput stgy t ohlc) (BacktestResult stgy t ohlc)) where
  
  toReport (TR.BacktestData _ (BacktestResult res)) = do
    Rep.subheader "Backtest"
    Experiment.render "Symbol at Close" "Backtest" res
 
 --------------------------------------------------------

instance OD.OHLCData (OptimizationInput stgy t ohlc) where
  type OHLCDataTy (OptimizationInput stgy t ohlc) = ohlc


instance OD.OHLCData (BacktestInput stgy t ohlc) where
  type OHLCDataTy (BacktestInput stgy t ohlc) = ohlc

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
        , Bin.limit = Just 4000
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

  (mcBegin, sample) <- getSymbol (Bin.USDT Bin.BTCUSDT)
  -- (mcBegin, sample) <- blackScholes

  let Signal.Sample inSamp outOfSamp = Signal.split 0.75 sample
  
      f (j, k) = (WindowSize j, WindowSize k)
      wins = map f (filter (uncurry (/=)) (liftA2 (,) [1 .. 100] [1 .. 100]))
      -- wins = map f (filter (uncurry (/=)) (liftA2 (,) [5 .. 10] [0 .. 10]))

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

  let -- analysis :: Ana.Analysis (OptimizationInput Short UTCTime Price) (BacktestInput UTCTime Price)
      analysis = Ana.Analysis {
        Ana.title = "Replaying Long Trades"
        , Ana.impulseGenerator = impulsesFromTwoMovingAverages unPrice
        , Ana.optimizationInput = OptimizationInput {
            optSample = inSamp
            , igInput = wins
            , optEquity = Equity (unPrice (snd (Signal.head inSamp)))
            , mcConfig = MCConfig {
                mcBars = 60*60*24
                , mcCount = MCCount 1000
                , mcBegin = mcBegin
                }
            , step = longStep
            -- , step = shortStep
            }
        , Ana.backtestInput = BacktestInput {
            btEquity = Equity (unPrice (snd (Signal.head outOfSamp)))
            , btSample = outOfSamp
            , btStep = longStep
            }
        }

      rep = Ana.analyze analysis

  t <- Rep.renderReport rep
  
  BSL.putStrLn t
 
