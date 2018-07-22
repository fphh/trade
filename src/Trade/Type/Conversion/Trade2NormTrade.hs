

module Trade.Type.Conversion.Trade2NormTrade where

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


trade2normTrade :: (UnOHLC a) => TradeList a -> NormTradeList
trade2normTrade (TradeList tl) =
  let g (_, old) (_, new) = Yield (unOHLC new / unOHLC old)
      
      f (Trade st ts) =
        let (t0, _) = shead "trades2normTrades" ts
            (t1, _) = slast "trades2normTrades" ts
            dur = t1 `diffUTCTime` t0
        in NormTrade st dur
           $ Vec.zipWith g ts (stail "trades2normTrades" ts)
           
  in NormTradeList (map f tl)
