

module Trade.Type.Signal.Yield where

import Trade.Type.Signal (Signal(..))
import Trade.Type.Yield (Yield, LogYield)


type YieldSignal t = Signal t Yield

type LogYieldSignal t = Signal t LogYield
