

module Trade.Type.ImpulseGenerator where

import Trade.Type.Signal.Impulse

type ImpulseGenerator inp = inp -> ImpulseSignal
