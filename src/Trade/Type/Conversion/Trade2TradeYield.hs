
module Trade.Type.Conversion.Trade2TradeYield where

import qualified Data.Vector as Vec

import Trade.Type.Yield (toYield)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.TradeYield (TradeYield(..), TradeYieldList(..))

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Help.SafeTail (stail)

trade2tradeYield ::
  (Type2Double ohlc) =>
  TradeList stgy t ohlc -> TradeYieldList
trade2tradeYield (TradeList tl) =
  let g (_, old) (_, new) = toYield (type2double new) (type2double old)
        
      f (Trade st ts) =
        let stl = stail "trades2normTrades tail" ts
        in TradeYield st (Vec.zipWith g ts stl)

  in TradeYieldList (map f tl)
