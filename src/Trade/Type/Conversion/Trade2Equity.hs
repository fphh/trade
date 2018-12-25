

module Trade.Type.Conversion.Trade2Equity where


import qualified Data.Vector as Vec

import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Help.SafeTail

{-
trade2equity :: (Type2Double a) => (ohlc -> a) -> Equity -> TradeList t ohlc -> EquitySignal t
trade2equity tradeAt (Equity eqty) (TradeList tl) =
  let g yys =
        let (tts, zs) = Vec.unzip yys
        in Vec.cons
           (shead "trade2equity" tts, 1)
           (Vec.zip (stail "trade2equity (1)" tts) (Vec.zipWith (/) (stail "trade2equity (2)" zs) zs))

      unCl (Trade NoPosition vs) = Vec.map (fmap (const 1.0)) vs
      unCl (Trade LongPosition vs) = Vec.map (fmap (type2double . tradeAt)) vs
      unCl _ = error "trade2equity: ShortPosition"
      
      cs = map (g . unCl) tl

      (ts, ys) = Vec.unzip (Vec.concat cs)

      ysNew = Vec.map Equity (stail "trade2equity" (Vec.scanl (*) eqty ys))
      
  in Signal (Vec.zip ts ysNew)
-}
