

module Trade.Type.Conversion.Trade2Equity where


import qualified Data.Vector as Vec

import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC, unOHLC)
import Trade.Type.State (State(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Help.SafeTail

trade2equity :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> EquitySignal
trade2equity tradeAt (Equity eqty) (TradeList tl) =
  let p (Trade NoPosition _) = False
      p _ = True

      g yys =
        let (tts, zs) = Vec.unzip yys
        in Vec.cons
           (shead "trade2equity" tts, 1)
           (Vec.zip (stail "trade2equity (1)" tts) (Vec.zipWith (/) (stail "trade2equity (2)" zs) zs))

      unCl (Trade _ vs) = Vec.map (fmap (unOHLC . tradeAt)) vs
      
      cs = map (g . unCl) (filter p tl)

      (ts, ys) = Vec.unzip (Vec.concat cs)

      ysNew = Vec.map Equity (stail "trade2equity" (Vec.scanl (*) eqty ys))
      
  in Signal (Vec.zip ts ysNew)

