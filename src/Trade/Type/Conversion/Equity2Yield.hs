

module Trade.Type.Conversion.Equity2Yield where

import Trade.Type.Yield (Yield(..), LogYield(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

class Equity2Yield yield where
  equity2yield :: Signal t Equity -> Signal t yield

instance Equity2Yield Yield where
  equity2yield = Signal.zipWith (\(Equity new) (Equity old) -> Yield (new / old)) mempty

instance Equity2Yield LogYield where
  equity2yield = Signal.zipWith (\(Equity new) (Equity old) -> LogYield (log (new / old))) mempty

