

module Trade.Type.DeltaSignal where

import Trade.Type.Bars (DeltaTy)
import Trade.Type.Delta (Delta(..))
import Trade.Type.Position (Position)
import Trade.Type.Signal (Signal)


data DeltaSignal t ohlc =
  DeltaSignal {
  start :: t
  , position :: Position
  , delta :: Signal (DeltaTy t) (Delta ohlc)
  }
