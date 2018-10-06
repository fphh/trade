{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Analysis.Report2 where

import qualified Data.Vector as Vec
--import Data.Vector (Vector)


import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.ByteString.Lazy.Char8 as BSL

-- import qualified Trade.TStatistics.TradeStatistics as TS

-- import Trade.Type.Equity ()
-- import Trade.Type.Yield ()

import Trade.Type.Fraction (Fraction, fullFrac)
import Trade.Type.Bars (Bars)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Yield (Yield)
import Trade.Type.History (History)
import qualified Trade.Type.Broom as Broom
import Trade.Type.Broom (Broom)
import Trade.Type.Signal (Signal (..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.OHLC (unOHLC, UnOHLC)
import Trade.Type.Trade (TradeList)
import Trade.Type.Conversion.Impulse2Trade (impulse2trade)
import Trade.Type.Conversion.Trade2NormTrade (trade2normTrade)
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Distribution (CDF)

import Trade.Timeseries.Url (ToUrl, toUrl)
-- import Trade.Timeseries.Row (RowInterface, closeR)
import Trade.Timeseries.OHLC

import Trade.Type.Signal.Impulse (ImpulseSignal)

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Backtest (backtest)
-- import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC

import Trade.Analysis.Risk (Risk, risk)
import Trade.Analysis.TWR (TWR, terminalWealthRelative)

import Trade.Report.Curve



import qualified Trade.Report.Report as Report

import Debug.Trace


data MCParams mcinput = MCParams {
  simBars :: Bars
  , monteCarloN :: Int
  , inSample :: mcinput
  }

data MCOutput rest = MCOutput {
  mcbroom :: Broom (History Yield)
  , mcrest :: rest
  }

class GetRest a where
  type RestTy a :: *
  broom :: a -> Broom (History Yield)
  rest :: a -> RestTy a
  

instance GetRest (MCOutput rest) where
  type RestTy (MCOutput rest) = rest
  broom = mcbroom
  rest = mcrest

type ImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal


data ReportInput mcinput mcoutput ohlc symbol trdAt = ReportInput {
  title :: String
  , symbol :: symbol
  , outOfSample :: PriceSignal ohlc
  , tradeAt :: ohlc -> trdAt
  , initialEquity :: Equity
  , step :: Fraction -> StepFunc
  , mcParams :: MCParams mcinput
  , generateImpulses :: ImpulseGenerator ohlc
  , montecarlo :: MCParams mcinput -> ImpulseGenerator ohlc -> IO mcoutput
  , fractions :: [Fraction]
  }


data ReportOutput mcoutput ohlc = ReportOutput {
  montecarloOutput :: mcoutput
  , riskByFraction :: [(Fraction, CDF Risk)]
  , twrByFraction :: [(Fraction, CDF TWR)]
  }


data Report mcinput mcoutput ohlc symbol trdAt = Report {
  reportInput :: ReportInput mcinput mcoutput ohlc symbol trdAt
  , reportOutput :: ReportOutput mcoutput ohlc
  }




-- createMC :: ReportInput mcinput mcoutput ohlc symbol trdAt -> IO mcoutput
-- createMC args = (montecarlo args) (mcParams args) (generateImpulses args)

analyze ::
  (UnOHLC trdAt, GetRest mcoutput) =>
  ReportInput mcinput mcoutput ohlc symbol trdAt -> IO (Report mcinput mcoutput ohlc symbol trdAt)
analyze repInp = do
  
  mcout <- montecarlo repInp (mcParams repInp) (generateImpulses repInp)

  let 
      brm = broom mcout
      mcp = mcParams repInp
      
      toTWR frac =
        let br = Broom.yield2equity ((step repInp) frac) (initialEquity repInp) brm
        in (frac, terminalWealthRelative (initialEquity repInp) br, risk br)

      twrs = map toTWR (fractions repInp)
      
  return $ Report {
    reportInput = repInp
    , reportOutput = ReportOutput {
        montecarloOutput = mcout
        , riskByFraction = map (\(f, _, r) -> (f, r)) twrs
        , twrByFraction = map (\(f, t, _) -> (f, t)) twrs
        }
    }



    

class ToReport a where
  toReport :: a -> [Report.ReportItem]

instance ToReport () where
  toReport _ = []

instance (ToReport a) => ToReport (Maybe a) where
  toReport x =
    case x of
      Nothing -> []
      Just y -> toReport y

instance (ToReport a) => ToReport [a] where
  toReport = concatMap toReport

instance (ToReport rest) => ToReport (MCOutput rest) where
  toReport mcout =
    let n = 20
        axisTitle str =
          let al = E.laxis_title E..~ str $ E.def
          in Report.AxisConfig al E.def Nothing
    in Report.svg (axisTitle "Bars") (axisTitle "Yield", Broom.broom2chart n (broom mcout))
       : toReport (rest mcout)

instance ToReport (Broom (History Equity)) where
    toReport eqties =
      let n = 20
          axisTitle str =
            let al = E.laxis_title E..~ str $ E.def
            in Report.AxisConfig al E.def Nothing
      in [Report.svg (axisTitle "Bars") (axisTitle "Equity", Broom.broom2chart n eqties)]

data CandleBars symbol ohlc = CandleBars {
  sym :: symbol
  , unCandleBars :: PriceSignal ohlc
  }

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


impulseAxisConf =
  let al = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
           $ E.def
      av = E.axis_show_labels E..~ False
           $ E.axis_show_ticks E..~ False
           $ E.def
      af = E.scaledAxis E.def (-1,10)
  in Report.AxisConfig al av (Just af)

axTitle str =
  let al = E.laxis_title E..~ str $ E.def
  in Report.AxisConfig al E.def Nothing



-- impulse2line :: ImpulseArgs -> ImpulseSignal -> Report.LineTyR UTCTime z Double
-- impulse2line args imps = Report.lineR "buy/sell" (impulse2line' args imps)


     
backtest2 ::
  (UnOHLC trdAt, ToUrl symbol) =>
  ReportInput mcinput mcoutput ohlc symbol trdAt -> Report.ReportItem
backtest2 input =
  let oos = outOfSample input
      impulses = generateImpulses input oos
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
      
  in Report.svgLR (axTitle "Time") (axTitle "Equity", [tickerLine, bt]) (impulseAxisConf, [inters])

  

-- Remove IO from here (IO comes from charting lib because of reading fonts?) !!!
render ::
  (ToReport mcoutput, GetRest mcoutput, OHLCInterface ohlc, ToUrl symbol, UnOHLC trdAt) =>
  Report mcinput mcoutput ohlc symbol trdAt -> IO BSL.ByteString
render report = do
  let repInp = reportInput report
      output = reportOutput report

      -- bt = backtest () (initialEquity reportInput)

      eqties = Broom.yield2equity (step repInp fullFrac) (initialEquity repInp) (broom (montecarloOutput output))

  let rep =
        Report.header (title repInp)
        :  Report.subheader "Out-of-Sample"
        :  toReport (CandleBars (symbol repInp) (outOfSample repInp))
        ++ [backtest2 repInp]
        ++ toReport (montecarloOutput output)
        ++ toReport eqties

  Report.renderReport (Report.report rep)






  
-- brm <- Broom.normHistoryBroom (simBars mcp) (monteCarloN mcp) ntrades 
-- qsTs = priceSignal repInp
-- impulses = (generateImpulses repInp) qsTs
-- trades = impulse2trade qsTs impulses
-- ntrades = trade2normTrade (fmap (tradeAt repInp) trades)





{-

 
prepareReport ::
  (OHLCInterface ohlc, ToYield ohlc) =>
  PriceSignal ohlc -> ReportInput ex ohlc trdAt -> IO (Report ex ohlc trdAt)
prepareReport qsTs args = do
  let impulses = (generateImpulses args) qsTs
      trades = impulses2trades qsTs impulses
      ntrades = trades2normTrades trades
      
  broom <- Broom.normHistoryBroom (simBars args) (monteCarloN args) ntrades

  let toTWR frac =
        let br = Broom.normEquityBroom ((step args) frac) (initialEquity args) broom
        in (frac, Broom.terminalWealthRelative (initialEquity args) br, Broom.risk br)

      twrs = map toTWR (fractions args)
  
      stats = TS.tradeStatistics ohlcClose trades
      
  return $ Report {
    reportInput = args
    , reportOutput = ReportOutput {
        priceSignal = qsTs
        , impulseSignal = impulses
        , tradeList = trades
        , broom = broom
        , terminalWealthRelative = twrs
        , tradeStatistics = stats
        }
    }


-}
