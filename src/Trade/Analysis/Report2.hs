{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Analysis.Report2 where

import qualified Data.Vector as Vec
--import Data.Vector (Vector)


import qualified Graphics.Rendering.Chart.Easy as E

import qualified Data.ByteString.Lazy.Char8 as BSL

-- import qualified Trade.TStatistics.TradeStatistics as TS

-- import Trade.Type.Equity ()
-- import Trade.Type.Yield ()

import Trade.Type.Fraction (Fraction)
import Trade.Type.Bars (Bars)
import Trade.Type.Equity (Equity)
import Trade.Type.History (History)
import Trade.Type.Broom (Broom, broom2chart)
import Trade.Type.Signal (Signal (..))
import Trade.Type.Signal.Price
import Trade.Type.OHLC (unOHLC)

import qualified Trade.Type.StepFunc as SF

import Trade.Timeseries.Quandl.Database (Symbol)
import Trade.Timeseries.Url (ToUrl, toUrl)
-- import Trade.Timeseries.Row (RowInterface, closeR)
import Trade.Timeseries.OHLC

import Trade.Render.Svg.Plot

import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Trade.TradeList

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Backtest
import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC



import qualified Trade.Report.Report as Report

import Debug.Trace


data MCParams mcinput = MCParams {
  simBars :: Bars
  , monteCarloN :: Int
  , input :: mcinput
  }

data MCOutput ey = MCOutput {
  broom :: Broom (History ey)
  }

type ImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal


data ReportInput mcinput mcoutput ohlc symbol {- trdAt -} = ReportInput {
  title :: String
  , symbol :: symbol
  , priceSignal :: PriceSignal ohlc
  -- , tradeAt :: ohlc -> trdAt
  , mcParams :: MCParams mcinput
  , generateImpulses :: ImpulseGenerator ohlc
  , montecarlo :: MCParams mcinput -> ImpulseGenerator ohlc -> IO (mcoutput Equity)
  , fractions :: [Fraction]
  }

createMC :: ReportInput mcinput mcoutput ohlc symbol {- trdAt -} -> IO (mcoutput Equity)
createMC args = (montecarlo args) (mcParams args) (generateImpulses args)

class MCReport a where
  toMCReport :: a -> [Report.ReportItem]

instance MCReport (MCOutput Equity) where
  toMCReport mcout =
    let n = 20
        axisTitle str =
          let al = E.laxis_title E..~ str $ E.def
          in Report.AxisConfig al E.def Nothing
    in [Report.svg (axisTitle "Bars") (axisTitle "Equity", broom2chart n (broom mcout))]

newtype CandleBars mcinput mcoutput ohlc symbol = CandleBars {
  unCandleBars :: ReportInput mcinput mcoutput ohlc symbol
  }

instance (ToUrl symbol, OHLCInterface ohlc) => MCReport (CandleBars mcinput mcoutput ohlc symbol) where
  toMCReport (CandleBars reportInput) =
    let toC (t, ohlc) =
          E.Candle t
          (unOHLC $ ohlcLow ohlc)
          (unOHLC $ ohlcOpen ohlc)
          0
          (unOHLC $ ohlcClose ohlc)
          (unOHLC $ ohlcHigh ohlc)
        toCandle (Signal ps) = Vec.map toC ps
    in [Report.candle (toUrl (symbol reportInput)) [toCandle (priceSignal reportInput)]]





renderReport ::
  (MCReport (mcoutput Equity), OHLCInterface ohlc, ToUrl symbol) =>
  ReportInput mcinput mcoutput ohlc symbol -> IO BSL.ByteString
renderReport reportInput = do
  mcout <- createMC reportInput

  let rep =
        Report.header (title reportInput)
        : toMCReport (CandleBars reportInput)
        ++ toMCReport mcout
  
  Report.renderReport (Report.report rep)
