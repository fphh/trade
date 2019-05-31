

module Trade.Type.DeltaSignal where

import Data.Time.Clock (UTCTime)

import Trade.Type.Delta (Delta(..))
import Trade.Type.Position (Position)
import Trade.Type.Signal (DeltaTimeseries)


data DeltaSignal ohlc =
  DeltaSignal {
  start :: UTCTime
  , position :: Position
  , delta :: DeltaTimeseries (Delta ohlc)
  }
