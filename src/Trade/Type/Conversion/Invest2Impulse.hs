

module Trade.Type.Conversion.Invest2Impulse where


import Trade.Type.Strategy (Long, Short)

import Trade.Type.Impulse (Impulse(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))

import Trade.Type.DisInvest (DisInvest(..), InvestSignal(..))



class Invest2Impulse stgy where
  invest2impulse :: InvestSignal t -> ImpulseSignal stgy t


instance Invest2Impulse Long where
  invest2impulse (InvestSignal m) =
    let f Invest = Buy
        f Disinvest = Sell
    in ImpulseSignal (fmap f m)



instance Invest2Impulse Short where
  invest2impulse (InvestSignal m) =
    let f Invest = Sell
        f Disinvest = Buy
    in ImpulseSignal (fmap f m)
