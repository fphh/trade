{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Conversion.Trade2NormTrade where


import qualified Data.Vector as Vec

import Trade.Type.Bars (Time, diff)

import Trade.Type.Yield (NoYield, ToYield, yield, noYield, toYield)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Help.SafeTail

import Debug.Trace

trade2normTrade ::
  (Type2Double ohlc, Time t, ToYield yield, Show t, Show ohlc, Show yield, Num yield, NoYield yield) =>
  TradeList t ohlc -> NormTradeList yield t
trade2normTrade (TradeList tl) =
  let g (_, old) (_, new) = toYield (type2double new) (type2double old)

      f (Trade st ts) =
        let (t0, _) = shead "trades2normTrades head" ts
            (t1, _) = slast "trades2normTrades last" ts
            stl = stail "trades2normTrades tail" ts
            dur = t1 `diff` t0
        in NormTrade st dur (Vec.zipWith g ts stl)

  in NormTradeList (map f tl)
