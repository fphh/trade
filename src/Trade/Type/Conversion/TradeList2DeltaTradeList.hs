

module Trade.Type.Conversion.TradeList2DeltaTradeList where

import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))

import Trade.Type.DeltaSignal.Algorithm (shortDeltaSignal, longDeltaSignal)

import Trade.Type.Strategy (Long, Short)
import Trade.Type.Trade (Trade(..), TradeList(..))
import Trade.Type.Delta (ToDelta)
import Trade.Type.Signal (Timeseries, Signal(..))



tradeList2DeltaTradeListHelper ::
  (ToDelta ohlc) =>
  (Timeseries ohlc -> DeltaSignal ohlc) -> TradeList stgy ohlc -> [DeltaSignal ohlc]
tradeList2DeltaTradeListHelper toDeltaSignal (TradeList tl) =
  let g (Trade pos ts) = (toDeltaSignal (Signal ts)) { position = pos }
  in map g tl


class TradeList2DeltaTradeList stgy where
  tradeList2DeltaTradeList :: (ToDelta ohlc) => TradeList stgy ohlc -> DeltaTradeList ohlc

instance TradeList2DeltaTradeList Long where
  tradeList2DeltaTradeList =
    DeltaTradeList . tradeList2DeltaTradeListHelper longDeltaSignal

instance TradeList2DeltaTradeList Short where
  tradeList2DeltaTradeList =
    DeltaTradeList . tradeList2DeltaTradeListHelper shortDeltaSignal
