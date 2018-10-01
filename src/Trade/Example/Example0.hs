{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Example.Example0 where

import Control.Applicative (liftA2)

import Text.Printf (printf)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import Data.Time.Clock
import Data.Time.Calendar.WeekDate

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Graphics.Rendering.Chart.Easy as E

import Data.Maybe (fromJust)

import Trade.Type.Equity
import Trade.Type.Yield
import Trade.Type.Fraction
import Trade.Type.Bars
import Trade.Type.OHLC
import Trade.Type.ImpulseGenerator (ImpulseGenerator, noImpulses)
import Trade.Type.Signal (Signal (..), split)
import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal.Price (PriceSignal)
import qualified Trade.Type.History as NH
import qualified Trade.Type.StepFunc as SF
import qualified Trade.Type.Broom as Broom
import Trade.Type.Impulse
import Trade.Type.Signal.Impulse
import Trade.Type.Conversion.Impulse2Trade (impulse2trade)
import Trade.Type.Conversion.Trade2NormTrade (trade2normTrade)

import Trade.Timeseries.Row
import Trade.Timeseries.OHLC

import Trade.Timeseries.Algorithm.Intersection

import Trade.Timeseries.Time
import Trade.Timeseries.Url (ToUrl, toUrl)

import Trade.Timeseries.Quandl.Quandl
import qualified Trade.Timeseries.Quandl.Database as DB
import Trade.Timeseries.Quandl.Database (Symbol (..))
import Trade.Timeseries.Quandl.FSE

-- import qualified Trade.Trade.TradeList as TL

import Trade.Help.SafeTail
import qualified Trade.Report.Report as Report

import Trade.Timeseries.Timeseries

import Trade.Algorithm.MovingAverage

-- import Trade.Analysis.Report2
import Trade.Analysis.Broom (normHistoryBroom)

import Trade.Analysis.Analysis (Analysis(..), analyze)
import Trade.Analysis.Optimize (NoOptimization(..))
import Trade.Analysis.Backtest (NoBacktest(..))
import qualified Trade.Analysis.Backtest as BT
import Trade.Analysis.ToReport (ToReport, toReport)


import Trade.Report.Curve (curve)


import Trade.Test.Data

import qualified Trade.Analysis.Analysis as Analysis

import Debug.Trace

{-
newRequest :: IO (DB.Symbol dbCode -> DB.RequestParams dbCode)
newRequest = do
  now <- getCurrentTime
  return $ \sym -> DB.RequestParams {
    DB.baseUrl = quandlBaseUrl
    , DB.symbol = sym
    , DB.apiKey = "hreY-Lysv3uyWPBiW6r_"
    , DB.from = parseDate "2006-01-01"
    -- , DB.from = parseDate "2014-01-01"
    -- , DB.to = parseDate "2018-05-20"
    , DB.to = Just now
    }

-- generateImpulseSignal :: (OHLCInterface ohlc) => Int -> Int -> PriceSignal ohlc -> ImpulseSignal
generateImpulseSignal :: (OHLCInterface ohlc) => Int -> Int -> ImpulseGenerator (PriceSignal ohlc)
generateImpulseSignal j k (Signal ps) =
  let f (t, x) = (t, unClose $ ohlcClose x)

      qs = Vec.map f ps

      avgJ = mavgTime j qs
      avgK = mavgTime k qs

      tradeSignal Down = Just Buy
      tradeSignal Up = Just Sell
      tradeSignal NoIntersection = Nothing

  in toImpulseSignal (\_ _ -> tradeSignal) (intersection avgJ avgK)

-- generateImpulseSignal2 :: (OHLCInterface ohlc) => ImpulseGenerator (PriceSignal ohlc)
generateImpulseSignal2 :: {- (OHLCInterface ohlc) => -} ImpulseGenerator (PriceSignal OHLC)
generateImpulseSignal2 (Signal ps) =
  let g (Open o) (Close c)
        | o < c = Sell
        | o >= c = Buy
      f (t, x) = (t, Just $ g (ohlcOpen x) (ohlcClose x))

  in Signal (Vec.map f ps)

{-
mc ::
  (OHLCInterface ohlc) => MCParams (PriceSignal ohlc) -> ImpulseGenerator ohlc -> IO (MCOutput ())
mc (MCParams simBars monteCarloN ps) gi = do
  let impulses = gi ps
      trades = impulse2trade ps impulses      
      ntrades = trade2normTrade (fmap ohlcClose trades)
  broom <- normHistoryBroom simBars monteCarloN ntrades
  return (MCOutput broom ()) -- (Broom.yield2equity (SF.stepFuncNoCommissionFullFraction) (Equity 100000) broom)
-}

data CandleBars symbol ohlc = CandleBars {
  sym :: symbol
  , unCandleBars :: PriceSignal ohlc
  }

class ToCandleBars a where
  type SymbolTy a :: *
  type OhlcTy a :: *
  toCandleBars :: a -> CandleBars (SymbolTy a) (OhlcTy a)

instance (ToUrl symbol, OHLCInterface ohlc) => ToReport (CandleBars symbol ohlc) where
  toReport (CandleBars sym sample) =
    let toC (t, ohlc) =
          E.Candle t
          (unOHLC $ ohlcLow ohlc)
          (unOHLC $ ohlcOpen ohlc)
          0
          (unOHLC $ ohlcClose ohlc)
          (unOHLC $ ohlcHigh ohlc)
        toCandle (Signal ps) = Vec.map toC ps
    in [Report.candle (toUrl sym) [toCandle sample]]

-----------------------------------------------

newtype RepString = RepString { str :: String }

instance ToReport RepString where
  toReport (RepString str) = [Report.text str]


-----------------------------------------------

 
data Backtest symbol ohlc = Backtest {
  symbol :: symbol
  , outOfSample :: PriceSignal ohlc
  }

instance ToCandleBars (Backtest symbol ohlc) where
  type SymbolTy (Backtest symbol ohlc) = symbol
  type OhlcTy (Backtest symbol ohlc) = ohlc
  toCandleBars bt = CandleBars (symbol bt) (outOfSample bt)


instance (ToUrl symbol, OHLCInterface ohlc) => BT.Backtest (Backtest symbol ohlc) where
  type BackTy (Backtest symbol ohlc) = BTResult symbol ohlc
  type ImpGenTy (Backtest symbol ohlc) = PriceSignal ohlc
  
  backtest impGen bt =
    let bla = 1

    
    in BTResult {
      candles = toCandleBars bt
      , test = RepString "Hi there, I'm Bobby Brown!"
      , impulses = ImpulseDiagram impGen bt -- (impGen (outOfSample bt))
      }

---------------------------------------------------------

data BTResult symbol ohlc = BTResult {
  candles :: CandleBars symbol ohlc
  , test :: RepString
  , impulses :: ImpulseDiagram symbol ohlc
  }
  
instance (ToUrl symbol, OHLCInterface ohlc) => ToReport (BTResult symbol ohlc) where
  toReport res =
    toReport (test res)
    ++ toReport (candles res)
    ++ toReport (impulses res)


--------------------------------------------------------

data ImpulseDiagram symbol ohlc = ImpulseDiagram {
  impGen :: ImpulseGenerator ohlc
  , impSig :: Backtest symbol ohlc
  }

axTitle :: (E.PlotValue a) => String -> Report.AxisConfig a
axTitle str =
  let al = E.laxis_title E..~ str $ E.def
  in Report.AxisConfig al E.def Nothing


impulseAxisConf :: Report.AxisConfig Double
impulseAxisConf =
  let al = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
           $ E.def
      av = E.axis_show_labels E..~ False
           $ E.axis_show_ticks E..~ False
           $ E.def
      af = E.scaledAxis E.def (-1,10)
  in Report.AxisConfig al av (Just af)


{-
instance ToReport ImpulseDiagram where
  toReport (ImpulseDiagram is) =
    let inters = Report.line "buy/sell" (curve is)
    in [Report.svg (axTitle "T") (axTitle "X", [inters])]
-}


instance ToReport (ImpulseDiagram symbol ohlc) where
  toReport (ImpulseDiagram impGen input) =
    let oos = outOfSample input
        impulses = impGen input oos
        trades = impulse2trade oos impulses
        bt = backtest (tradeAt input) (initialEquity input) trades

        Equity ie = initialEquity input
      
        (_, firstPrice) = Vec.head (unSignal oos)
        fp = unEquity (initialEquity input) / (unOHLC (tradeAt input firstPrice))
      
        tickerLine =
          Report.lineL
          (toUrl (symbol input))
          (Vec.map (fmap ((fp*) . unOHLC . tradeAt input)) (unSignal oos))
  
        inters = Report.lineR "buy/sell" (curve impulses)
        
    in [Report.svgLR (axTitle "Time") (axTitle "Equity", [tickerLine, bt]) (impulseAxisConf, [inters])]



--------------------------------------------------------



mainDoIt :: (ToUrl symbol, Show symbol) => symbol -> PriceSignal OHLC -> IO ()
mainDoIt sym qs = do

  writeFile "/tmp/test.txt" (show (sym, qs))
  
  let sample = split 0.75 qs

      backtest = Backtest {
        symbol = sym
        , outOfSample = Signal.outOfSample sample
        }

  let analysis = Analysis {
        impulseGenerator = generateImpulseSignal2
        , optimizationInp = NoOptimization
        , backtestInp = backtest
        }

      rep = analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t

  
  {-
  let sample = split 0.75 qs

      mcParams = MCParams {
        simBars = Bars 500
        , monteCarloN = 1000
        , inSample = Signal.inSample sample
        }

      inArgs = ReportInput {
        title = "This is the Report"
        , symbol = sym
        , outOfSample = Signal.outOfSample sample
        , tradeAt = _ohlcClose
        , initialEquity = Equity 100000
        , step = SF.stepFuncNoCommission
        , mcParams = mcParams
        , generateImpulses = generateImpulseSignal 17 8
        , montecarlo = mc
        , fractions = map Fraction (0.05 : [0.2, 0.4 .. 2])
        }

  -- mcout <- createMC inArgs

  report <- analyze inArgs
  
  str <- render report
  
  BSL.putStrLn str
-}


mainFile :: FilePath -> IO ()
mainFile path = do
  (sym :: Symbol FSE, qs) <- fmap read (readFile path)
  mainDoIt sym qs


mainNetwork :: IO ()
mainNetwork = do

  let raa = Symbol FSE RAA_X
      tui = Symbol FSE TUI1_X
      zooplus = Symbol FSE ZO1_X
      allianz = Symbol FSE ALV_X
      bmw = Symbol FSE BMW_X

      sym = raa
  
  req <- newRequest
  qsTs <- getTicker req sym

  let vs = Vec.map (\x -> (dateDI x, prepareData x)) (Vec.filter isIncomplete qsTs)

  mainDoIt sym (Signal vs)

example0 :: Maybe FilePath -> IO ()
example0 x =
  case x of
    Just path -> mainFile path
    Nothing -> mainNetwork




  

{-
mainFile :: FilePath -> IO ()
mainFile path = do
      
  txt <- readFile path

  let f :: String -> (UTCTime, OHLC)
      f str =
        let (t, _:c) = List.span (/=',') str
            p = read c
        in (read t, OHLC  (Open p) (High p) (Low p) (Close p) (Volume 100))

      raa = Symbol FSE RAA_X

  
      vs = Signal $ Vec.fromList $ map f (lines txt)

  mainDoIt raa vs


  -- BSL.putStrLn (renderStats reps)

  -- BSL.putStrLn (renderReport
-}


{-
prepareData :: (RowInterface row) => row -> OHLC
prepareData r = OHLC {
  _ohlcOpen = maybe (error "") id (openR r)
  , _ohlcHigh = maybe (error "") id (highR r)
  , _ohlcLow = maybe (error "") id (lowR r)
  , _ohlcClose = maybe (error "") id (closeR r)
  , _ohlcVolume = maybe (error "") id (volumeR r)
  }

isIncomplete :: (RowInterface row) => row -> Bool
isIncomplete r =
  let b = openR r >> highR r >> lowR r >> closeR r >> volumeR r
  in b /= Nothing

  
instance ToYield OHLC where
  forwardYield x y = forwardYield (_ohlcClose x) (_ohlcClose y)
-}
{-
type StepFunc = Fraction -> Equity -> Yield -> Equity


stepFunc :: StepFunc
stepFunc (Fraction frac) (Equity e) (Yield y) =
  let e0 = frac * e
      e1 = (1-frac) * e
  in Equity (e1 + (e0*y))
-}


{-
mainDoIt :: (OHLCInterface ohlc, ToYield ohlc) => Symbol FSE -> PriceSignal ohlc -> IO ()
mainDoIt sym qs = do
  
  let inArgs = ReportInput {
        title = printf "Strategy Analysis"
        , description = "Description (TODO)"
        , symbol = sym
        -- , simBars = Bars 500
        , tradeAt = ohlcClose
        , normEquityBroom = createBroom qs
        -- , initialEquity = Equity 100000
        -- , monteCarloN = 100
        -- , generateImpulses = generateImpulseSignal 14 21
        -- , step = stepFunc
        , fractions = map Fraction (0.05 : [0.2, 0.4 .. 2])
        }

  report <- prepareReport qs inArgs
  str <- renderReport report

  -- str <- renderExtendedReport report
  
  BSL.putStrLn str
-}
-}
