

module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Trade.TradeList

import Trade.Type.EquityAndShare

import Trade.Render.Svg.Plot

backtest :: Equity -> TradeList Close -> PlotItem Vector UTCTime
backtest eqty trades = Line "backtest" (Vec.map (fmap unEquity) (trade2equity eqty trades))
