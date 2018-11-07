{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Conversion.Trade2NormTrade where


import qualified Data.Vector as Vec

import Trade.Type.Bars (Time, diff)

import Trade.Type.Yield (Yield(..))
import Trade.Type.OHLC (UnOHLC, unOHLC)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Trade.Help.SafeTail


trade2normTrade :: (UnOHLC ohlc, Time t) => TradeList t ohlc -> NormTradeList t
trade2normTrade (TradeList tl) =
  let g (_, old) (_, new) = Yield (unOHLC new / unOHLC old)

      f (Trade st ts) =
        let (t0, _) = shead "trades2normTrades head" ts
            (t1, _) = slast "trades2normTrades last" ts
            tl = stail "trades2normTrades tail" ts
            dur = t1 `diff` t0
        in NormTrade st dur (Vec.zipWith g ts tl)
        
  in NormTradeList (map f tl)
