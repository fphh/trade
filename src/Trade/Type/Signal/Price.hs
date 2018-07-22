
module Trade.Type.Signal.Price where

import Data.Time.Clock (UTCTime)

import Trade.Type.Signal (Signal)


type PriceSignal ohcl = Signal UTCTime ohcl

