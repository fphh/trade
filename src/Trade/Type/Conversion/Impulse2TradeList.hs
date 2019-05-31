

module Trade.Type.Conversion.Impulse2TradeList where

import qualified Data.Vector as Vec
import qualified Data.Map as Map


import Trade.Type.Position (Position(..))

import Trade.Type.Signal (Timeseries, Signal(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Impulse (Impulse(..))

import Trade.Type.Strategy (Long, Short)
import Trade.Type.Strategy.Index (Index(..))

class Impulse2TradeList stgy where
  impulse2tradeList :: (Show ohlc) => Timeseries ohlc -> ImpulseSignal stgy -> TradeList stgy ohlc


longTag :: Impulse -> Position
longTag Buy = Invested
longTag Sell = NotInvested

shortTag :: Impulse -> Position
shortTag Buy = NotInvested
shortTag Sell = Invested


impulse2tradeListHelp ::
  (Impulse -> Position) -> Timeseries ohlc -> ImpulseSignal stgy -> TradeList stgy ohlc
impulse2tradeListHelp tag (Signal ps) (ImpulseSignal is) =
  let idxs = Map.toList is
      f (Index i0, imp) (Index i1, _) = Trade (tag imp) (Vec.slice i0 (i1-i0+1) ps)
  in TradeList (zipWith f idxs (tail idxs))


instance Impulse2TradeList Long where
  impulse2tradeList = impulse2tradeListHelp longTag

instance Impulse2TradeList Short where
  impulse2tradeList = impulse2tradeListHelp shortTag 

