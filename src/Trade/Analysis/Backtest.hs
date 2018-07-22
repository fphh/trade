

module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec


import Trade.Type.Signal (unSignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC)
import Trade.Type.Trade (TradeList)
import Trade.Type.Conversion.Trade2Equity (trade2equity)

import qualified Trade.Report.Report as Report

backtest :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Report.LineTyL UTCTime Double z
backtest tradeAt eqty trades =
  Report.lineL "backtest" (Vec.map (fmap unEquity) (unSignal (trade2equity tradeAt eqty trades)))
