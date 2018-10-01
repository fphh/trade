

module Trade.Type.Conversion.Trade2NormTrade where

import Data.Time.Clock (diffUTCTime)

import qualified Data.Vector as Vec

import Trade.Type.Yield (Yield(..))
import Trade.Type.OHLC (UnOHLC, unOHLC)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Trade.Help.SafeTail


trade2normTrade :: (UnOHLC ohlc) => TradeList ohlc -> NormTradeList
trade2normTrade (TradeList tl) =
  let g (_, old) (_, new) = Yield (unOHLC new / unOHLC old)
      
      f (Trade st ts) =
        let (t0, _) = shead "trades2normTrades" ts
            (t1, _) = slast "trades2normTrades" ts
            dur = t1 `diffUTCTime` t0
        in NormTrade st dur
           $ Vec.zipWith g ts (stail "trades2normTrades" ts)
           
  in NormTradeList (map f tl)