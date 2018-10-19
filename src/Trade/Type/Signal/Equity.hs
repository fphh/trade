
module Trade.Type.Signal.Equity where

import Trade.Type.Signal (Signal)
import Trade.Type.Equity (Equity)


type EquitySignal t = Signal t Equity

