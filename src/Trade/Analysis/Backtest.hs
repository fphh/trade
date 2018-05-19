

module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Trade.State
import Trade.Trade.TradeList

import Trade.Type.EquityAndShare

import Trade.Render.Svg.Plot


backtest' :: Equity -> TradeList Close -> Vector (UTCTime, Double)
backtest' (Equity eqty) (TradeList tl) =
  let p (Trade NoPosition _) = False
      p _ = True

      g ys =
        let (ts, zs) = Vec.unzip ys
        in Vec.cons (Vec.head ts, 1) (Vec.zip (Vec.tail ts) (Vec.zipWith (/) (Vec.tail zs) zs))

      unCl (Trade _ vs) = Vec.map (fmap unClose) vs
      
      cs = map (g . unCl) (filter p tl)

      (ts, ys) = Vec.unzip (Vec.concat cs)
      
      ysNew = Vec.tail (Vec.scanl (*) eqty ys)
      
  in Vec.zip ts ysNew


backtest :: Equity -> TradeList Close -> PlotItem Vector UTCTime
backtest eqty trades = Line "backtest" (backtest' eqty trades)
