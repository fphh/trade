
module Trade.Type.DeltaTradeList where

import Trade.Type.DeltaSignal

data DeltaTradeList t ohlc = DeltaTradeList {
  unDeltaSignals :: [DeltaSignal t ohlc]
  }

