{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Trade.Example.ReplayingTrades where

import Control.Applicative (liftA2)

import Data.Time.Clock (UTCTime(..), NominalDiffTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Calendar (fromGregorian)

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.Vector as Vec

import Trade.Type.BarLength (BarLength(..), barLength2diffTime)
import Trade.Type.Broom (Broom) -- , broom2chart)
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
import Trade.Type.Sample (Sample(..))
import qualified Trade.Type.Sample as Sample
import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.Step (StepTy(LongStep, ShortStep), longFraction, shortFraction, longCommission, shortCommission, shortInterests)
import Trade.Type.Strategy (Long, Short)

import qualified Trade.Type.Experiment as Experiment
import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList)
import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList)
import Trade.Type.Conversion.Invest2Impulse (Invest2Impulse, invest2impulse)
import Trade.Type.Yield (ToYield)

-- import Trade.Algorithm.MovingAverage (WindowSize(..))
import Trade.Strategy.Type (Window(..), K(..))
import Trade.Strategy.Library.MovingAverages (movingAverages, stdBreakout)

import Trade.Statistics.Statistics ()

import qualified Trade.Analysis.TWR as TWR
import qualified Trade.Analysis.Risk as Risk
import qualified Trade.Analysis.OHLCData as OD
import qualified Trade.Analysis.Report as ARep
import qualified Trade.Analysis.Optimize as Opt
import qualified Trade.Analysis.Analysis as Ana
import qualified Trade.Analysis.Backtest as BT


import Trade.Report.Basic (header, subheader, subsubheader, text)
import qualified Trade.Report.Heatmap as Heat
import Trade.Report.HtmlReader (render)
import qualified Trade.Report.Style as Style
import qualified Trade.Report.Table as Tab
import qualified Trade.Report.ToReport as TR
import Trade.Report.Line (Line)

import Trade.Report.Pretty (Pretty)

import Trade.MonteCarlo.ResampleTrades.Broom (MCConfig(..), mc, MCCount(..))
import Trade.MonteCarlo.Simulation.BlackScholes (Mu(..), Sigma(..), blackScholesDet)

import Trade.Statistics.Algorithm (Statistics)

import qualified Trade.Test.Time as T

import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Database as Bin
import qualified Trade.Timeseries.Binance.Interval as Bin
import qualified Trade.Timeseries.Binance.Symbol as Bin
import qualified Trade.Timeseries.Url as Url


data Symbol = ASym deriving (Show, Eq, Ord)


data OptimizationInput stgy sym ohlc = OptimizationInput {
  optSample :: Map sym (Timeseries ohlc)
  , igInput :: [(Window, Window)]
  , mcConfig :: MCConfig
  , optEquity :: Equity
  , barLength :: BarLength
  , step :: StepTy stgy
  }

  
data OptimizationResult sym stgy = OptimizationResult {
  result :: Experiment.Result stgy sym Price
  , lastEquities :: Map (Window, Window) (Map sym Equity)
  }



instance ( Ord sym
         , Show sym
         , Invest2Impulse stgy
         , TradeList2DeltaTradeList stgy
         , Impulse2TradeList stgy
         , StepFunction (StepTy stgy)) =>
  Opt.Optimize (OptimizationInput stgy sym Price) where

  type OptReportTy (OptimizationInput stgy sym Price) = OptimizationResult sym stgy
  type OptInpTy (OptimizationInput stgy sym Price) = (Window, Window)

  optimize (ImpulseGenerator strat) optInp =
    let findBestWinSize winSize acc =
          let e = Experiment.Input (step optInp) (optEquity optInp) (barLength optInp) (strat winSize) (optSample optInp)
          in Map.insert winSize (Experiment.conduct e) acc

        p e0 e1 = compare (Experiment.lastEquities e1) (Experiment.lastEquities e0)

        strats = List.foldr findBestWinSize Map.empty (igInput optInp)

        f (Experiment.Result inp _) = Experiment.impulseGenerator inp
        sortedStarts@(optStrat:_) = map f (List.sortBy p (Map.elems strats))
 
        expmnt = Experiment.Input (step optInp) (optEquity optInp) (barLength optInp) optStrat (optSample optInp)
        res = Experiment.conduct expmnt
    
    in (RankedStrategies sortedStarts, OptimizationResult res (fmap Experiment.lastEquities strats))


instance ( Show sym
         , Ord sym
         , StepFunction (StepTy stgy)) =>
  TR.ToReport (ARep.OptimizationData (OptimizationInput stgy sym Price) (OptimizationResult sym stgy)) where

  toReport (ARep.OptimizationData optInp (OptimizationResult res lastEqties)) =
    let addendum sym = do
          let eqty = unEquity (optEquity optInp)
              g (Window y, Window x) = (y, x)
              ms = Map.mapKeys g (fmap (fmap unEquity . Map.lookup sym) lastEqties)
          subsubheader "Heatmap"
          Heat.heatmap eqty ms
    in Experiment.render addendum res


data BacktestInput stgy sym ohlc = BacktestInput {
  btEquity :: Equity
  , btSample :: Map sym (Timeseries ohlc)
  , btBarLength :: BarLength
  , btStep :: StepTy stgy
  }
  
data BacktestResult stgy sym ohlc = BacktestResult (Experiment.Result stgy sym ohlc)
                          
instance ( ToDelta ohlc
         , Ord sym
         , Show ohlc
         , TradeList2DeltaTradeList stgy
         , Impulse2TradeList stgy
         , Invest2Impulse stgy
         , StepFunction (StepTy stgy)) => BT.Backtest (BacktestInput stgy sym ohlc) where
  
  type BacktestReportTy (BacktestInput stgy sym ohlc) = BacktestResult stgy sym ohlc

  backtest (NonEmptyList optStrat _) (BacktestInput initEqty ps bl step) =
    let expmnt = Experiment.Input step initEqty bl optStrat ps
    in BacktestResult (Experiment.conduct expmnt)

instance (Show sym
         , Ord sym
         , StepFunction (StepTy stgy)
         , E.PlotValue ohlc
         , Pretty ohlc
         , Eq ohlc
         , Floating ohlc
         , Statistics ohlc
         , ToYield ohlc
         , Line (Timeseries ohlc)
         , Line (Vec.Vector (UTCTime, ohlc))) =>
  TR.ToReport (ARep.BacktestData (BacktestInput stgy sym ohlc) (BacktestResult stgy sym ohlc)) where
  
  toReport (ARep.BacktestData _ (BacktestResult res)) = do
    header "Backtest"
    Experiment.render (const (return ())) res
 
 --------------------------------------------------------

instance OD.OHLCData (OptimizationInput stgy sym ohlc) where
  type OHLCDataTy (OptimizationInput stgy sym ohlc) = ohlc


instance OD.OHLCData (BacktestInput stgy sym ohlc) where
  type OHLCDataTy (BacktestInput stgy sym ohlc) = ohlc

--------------------------------------------------------

barLen :: BarLength
barLen = Hour 1
-- barLen = Min 1


getSymbol :: Bin.Symbol -> IO (UTCTime, Timeseries Price)
getSymbol sym = do

  now <- getCurrentTime
  
  let req = Bin.RequestParams {
        Bin.baseUrl = Bin.binanceBaseUrl
        , Bin.symbol = sym
        , Bin.interval = Bin.Interval barLen
        , Bin.limit = Just 1000
        , Bin.from = Nothing
        , Bin.to = Just now -- ((fromIntegral (negate (10*24*60*60))) `addUTCTime` now)
        }

      toSignal row = (Bin.toDate row, Price (unClose (Bin.close row)))

      mcBegin = UTCTime (fromGregorian 2020 1 1) 0
  
  fmap ((\x -> (mcBegin, x)) . Signal . Vec.map toSignal) (Bin.getTicker req)

{-
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
-}


example :: IO ()
example = do

  let sym = Bin.USDT Bin.BTCUSDT
  (mcBegin, timeseries) <- getSymbol sym
  -- (mcBegin, sample) <- blackScholes

  let sample = Sample.split 0.75 timeseries
  
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
  
      shortStep :: StepTy Short
      shortStep = ShortStep {
        shortFraction = Fraction 1
        , shortCommission = Commission (const 0)
        , shortInterests = Interests (interests rtf 0.0)
        }

  let analysis ::
        Sample Price
        -> Ana.Analysis (OptimizationInput Long Bin.Symbol Price) (BacktestInput Long Bin.Symbol Price)
        
      analysis (Sample inSamp outOfSamp) = Ana.Analysis {
        Ana.title = "This is the strategy name"
        , Ana.impulseGenerator = IG.ImpulseGenerator (\(j, k) -> IG.OptimizedImpulseGenerator (movingAverages j k))

        , Ana.optimizationInput = OptimizationInput {
            optSample = Map.fromList [(sym, inSamp)]
            , igInput = wins
            , optEquity = Equity (unPrice (snd (Signal.head inSamp)))
            , barLength = barLen
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
            , btSample = Map.fromList [(sym, outOfSamp)]
            , btBarLength = barLen
            , btStep = longStep
            }
        }

      rep = Ana.analyze (analysis sample)

  t <- render rep
  
  BSL.putStrLn t

