

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec


import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal (Signal(..))


type ImpulseGenerator inp = inp -> ImpulseSignal

noImpulses :: ImpulseGenerator (PriceSignal ohlc)
noImpulses (Signal ps) = Signal (Vec.map (fmap (const Nothing)) ps)
