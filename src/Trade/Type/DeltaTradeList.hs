
module Trade.Type.DeltaTradeList where

import Trade.Type.DeltaSignal (DeltaSignal)

data DeltaTradeList ohlc = DeltaTradeList {
  unDeltaTradeList :: [DeltaSignal ohlc]
  }

