

module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec

import qualified Trade.Report.Report as Report

import Trade.Trade.TradeList

import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC)

backtest :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Report.LineTyL UTCTime Double z
backtest tradeAt eqty trades =
  Report.lineL "backtest" (Vec.map (fmap unEquity) (trade2equity tradeAt eqty trades))
