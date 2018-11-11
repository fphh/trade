


module Trade.Type.Conversion.Price2Yield where

import Trade.Type.Yield (Yield, LogYield, toYield)
import Trade.Type.Price (Price(..))

import Trade.Type.Signal (Signal)
import qualified Trade.Type.Signal as Signal


class Price2Yield yield where
  price2yield ::  Signal t Price -> Signal t yield


instance Price2Yield Yield where
    price2yield = Signal.zipWith (\(Price x) (Price y) -> toYield x y) mempty

instance Price2Yield LogYield where
  price2yield = Signal.zipWith (\(Price x) (Price y) -> toYield x y) mempty

  
