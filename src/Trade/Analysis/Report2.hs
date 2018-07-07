{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Report2 where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Printf (printf)

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Trade.TStatistics.TradeStatistics as TS

-- import Trade.Type.Equity ()
-- import Trade.Type.Yield ()

import Trade.Type.Fraction (Fraction)
import Trade.Type.Bars (Bars)
import Trade.Type.Equity (Equity)
import Trade.Type.History (History)

import Trade.Timeseries.Quandl.Database (Symbol)
import Trade.Timeseries.Url (ToUrl, toUrl)
-- import Trade.Timeseries.Row (RowInterface, closeR)
import Trade.Timeseries.OHLC

import Trade.Render.Svg.Plot

import Trade.Trade.ImpulseSignal
import Trade.Trade.PriceSignal
import Trade.Trade.TradeList

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Broom (Broom)
import Trade.Analysis.Backtest
import qualified Trade.Analysis.MonteCarlo as MC

import qualified Trade.Analysis.StepFunc as SF


import qualified Trade.Report.Report as Report

import Debug.Trace


data MCParams mcinput = MCParams {
  simBars :: Bars
  , monteCarloN :: Int
  , input :: mcinput
  }

data MCOutput ey = MCOutput {
  broom :: Broom.Broom (History ey)
  }

type ImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal ohlc


data ReportInput mcinput mcoutput ohlc {- trdAt -} = ReportInput {
  priceSignal :: PriceSignal ohlc
  -- , tradeAt :: ohlc -> trdAt
  , mcParams :: MCParams mcinput
  , generateImpulses :: ImpulseGenerator ohlc
  , montecarlo :: MCParams mcinput -> ImpulseGenerator ohlc -> IO (mcoutput Equity)
  , fractions :: [Fraction]
  }

createMC :: ReportInput mcinput mcoutput ohlc {- trdAt -} -> IO (mcoutput Equity)
createMC args = (montecarlo args) (mcParams args) (generateImpulses args)

class MCReport a where
  toMCReport :: a -> [Report.ReportItem]

instance MCReport (MCOutput Equity) where
  toMCReport mcout =
    let  axisTitle str =
           let al = E.laxis_title E..~ str $ E.def
           in Report.AxisConfig al E.def Nothing
    in [Report.svg (axisTitle "Bars") (axisTitle "Equity", Broom.broom2chart 20 (broom mcout))]
