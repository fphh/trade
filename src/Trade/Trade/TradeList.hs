

module Trade.Trade.TradeList where



import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing)

import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.Yield (Yield(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC, unOHLC)
import Trade.Type.State (State(..))
import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Trade.Help.SafeTail



trade2equity :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Vector (UTCTime, Equity)
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
      
  in Vec.zip ts ysNew
