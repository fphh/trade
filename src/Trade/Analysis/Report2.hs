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
import Trade.Type.Equity (Equity)
import Trade.Type.Yield (Yield)
import Trade.Type.History (History)
import qualified Trade.Type.Broom as Broom
import Trade.Type.Broom (Broom)
import Trade.Type.Signal (Signal (..))
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
--import Trade.Analysis.Backtest
-- import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC

import Trade.Analysis.Risk (Risk, risk)
import Trade.Analysis.TWR (TWR, terminalWealthRelative)



import qualified Trade.Report.Report as Report

-- import Debug.Trace


data MCParams mcinput = MCParams {
  simBars :: Bars
  , monteCarloN :: Int
  , input :: mcinput
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
  , priceSignal :: PriceSignal ohlc
  , tradeAt :: ohlc -> trdAt
  , initialEquity :: Equity
  , step :: Fraction -> StepFunc
  , mcParams :: MCParams mcinput
  , generateImpulses :: ImpulseGenerator ohlc
  , montecarlo :: MCParams mcinput -> ImpulseGenerator ohlc -> IO mcoutput
  , fractions :: [Fraction]
  }


data ReportOutput mcoutput ohlc = ReportOutput {
  impulseSignal :: ImpulseSignal
  , tradeList :: TradeList ohlc
  , riskByFraction :: [(Fraction, CDF Risk)]
  , twrByFraction :: [(Fraction, CDF TWR)]
  , montecarloOutput :: mcoutput
  }


data Report mcinput mcoutput ohlc symbol trdAt = Report {
  reportInput :: ReportInput mcinput mcoutput ohlc symbol trdAt
  , reportOutput :: ReportOutput mcoutput ohlc
  }

createMC :: ReportInput mcinput mcoutput ohlc symbol trdAt -> IO mcoutput
createMC args = (montecarlo args) (mcParams args) (generateImpulses args)

analyze ::
  (UnOHLC trdAt, GetRest mcoutput) =>
  ReportInput mcinput mcoutput ohlc symbol trdAt -> IO (Report mcinput mcoutput ohlc symbol trdAt)
analyze repInp = do
  
  mcout <- createMC repInp

  let -- qsTs = priceSignal repInp
      -- impulses = (generateImpulses repInp) qsTs
      -- trades = impulse2trade qsTs impulses
      -- ntrades = trade2normTrade (fmap (tradeAt repInp) trades)

      brm = broom mcout
      mcp = mcParams repInp
      
  -- brm <- Broom.normHistoryBroom (simBars mcp) (monteCarloN mcp) ntrades

  let toTWR frac =
        let br = Broom.yield2equity ((step repInp) frac) (initialEquity repInp) brm
        in (frac, terminalWealthRelative (initialEquity repInp) br, risk br)

      twrs = map toTWR (fractions repInp)
      
  return $ Report {
    reportInput = repInp
    , reportOutput = ReportOutput {
        -- impulseSignal = impulses
        -- , tradeList = trades
        montecarloOutput = mcout
        , riskByFraction = map (\(f, _, r) -> (f, r)) twrs
        , twrByFraction = map (\(f, t, _) -> (f, t)) twrs
        }
    }

class ToReport a where
  toReport :: a -> [Report.ReportItem]

instance ToReport () where
  toReport _ = []

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

newtype CandleBars mcinput mcoutput ohlc symbol trdAt = CandleBars {
  unCandleBars :: ReportInput mcinput mcoutput ohlc symbol trdAt
  }

instance (ToUrl symbol, OHLCInterface ohlc) => ToReport (CandleBars mcinput mcoutput ohlc symbol trdAt) where
  toReport (CandleBars repInp) =
    let toC (t, ohlc) =
          E.Candle t
          (unOHLC $ ohlcLow ohlc)
          (unOHLC $ ohlcOpen ohlc)
          0
          (unOHLC $ ohlcClose ohlc)
          (unOHLC $ ohlcHigh ohlc)
        toCandle (Signal ps) = Vec.map toC ps
    in [Report.candle (toUrl (symbol repInp)) [toCandle (priceSignal repInp)]]



-- Remove IO from here (IO comes from charting lib because of reading fonts?) !!!
render ::
  (ToReport mcoutput, GetRest mcoutput, OHLCInterface ohlc, ToUrl symbol) =>
  Report mcinput mcoutput ohlc symbol trdAt -> IO BSL.ByteString
render report = do
  let repInp = reportInput report
      output = reportOutput report
  -- let bt = backtest () (initialEquity reportInput)

      eqties = Broom.yield2equity (step repInp fullFrac) (initialEquity repInp) (broom (montecarloOutput output))

  let rep =
        Report.header (title repInp)
        : toReport (CandleBars repInp)
        ++ toReport (montecarloOutput output)
        ++ toReport eqties

  Report.renderReport (Report.report rep)










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
