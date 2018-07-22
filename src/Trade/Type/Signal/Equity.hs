
module Trade.Type.Signal.Equity where

import Data.Time.Clock (UTCTime)

import Trade.Type.Signal (Signal)
import Trade.Type.Equity (Equity)


type EquitySignal = Signal UTCTime Equity

