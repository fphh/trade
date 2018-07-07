
module Trade.Analysis.Report2 where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Printf (printf)

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Trade.TStatistics.TradeStatistics as TS

import Trade.Type.EquityAndShare
import Trade.Type.Yield
import Trade.Type.Fraction

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
import Trade.Analysis.NormHistory
import Trade.Analysis.Backtest
import qualified Trade.Analysis.MonteCarlo as MC
import Trade.Analysis.Bars

import qualified Trade.Analysis.StepFunc as SF


import qualified Trade.Report.Report as Report

import Debug.Trace


data MCParams = MCParams {
  simBars :: Bars
  , monteCarloN :: Int
  }

data MCOutput ohlc = MCOutput {
  broom :: Broom.Broom (NormEquityHistory ohlc)
  }

type ImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal ohlc


data ReportInput mcoutput ohlc = ReportInput {
  priceSignal :: PriceSignal ohlc
  , mcParams :: MCParams
  , generateImpulses :: ImpulseGenerator ohlc
  , montecarlo :: PriceSignal ohlc -> MCParams -> ImpulseGenerator ohlc -> IO (mcoutput ohlc)
  , fractions :: [Fraction]
  }

createMC :: ReportInput mcoutput ohlc -> IO (mcoutput ohlc)
createMC args = (montecarlo args) (priceSignal args) (mcParams args) (generateImpulses args)

class MCReport a where
  toMCReport :: a -> [Report.ReportItem]

instance MCReport (MCOutput ohlc) where
  toMCReport mcout =
    let  axisTitle str =
           let al = E.laxis_title E..~ str $ E.def
           in Report.AxisConfig al E.def Nothing
    in [Report.svg (axisTitle "Bars") (axisTitle "Equity", Broom.broom2chart 20 (broom mcout))]
