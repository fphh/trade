

module Trade.Type.Conversion.Yield2Equity where

import Trade.Type.Yield (Yield, LogYield)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.StepFunc (StepFunc)

class Yield2Equity yield where
  yield2equity :: StepFunc yield -> Equity -> Signal t yield -> Signal t Equity
  yield2equity step eqty ps = Signal.scanl step eqty ps

instance Yield2Equity Yield
instance Yield2Equity LogYield
