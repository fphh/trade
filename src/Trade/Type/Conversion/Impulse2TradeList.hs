

module Trade.Type.Conversion.Impulse2TradeList where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.List as List

import Trade.Type.Position (Position(..))


import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Impulse (Impulse(..))

import Trade.Type.Strategy (Long, Short)
import Trade.Type.Strategy.Index (Index(..))

import Trade.Help.SafeTail (shead, stail)

import Debug.Trace

class Impulse2TradeList stgy where
  impulse2tradeList :: (Ord t, Show t, Show ohlc) => Signal t ohlc -> ImpulseSignal stgy -> TradeList stgy t ohlc


longTag :: Impulse -> Position
longTag Buy = Invested
longTag Sell = NotInvested

shortTag :: Impulse -> Position
shortTag Buy = NotInvested
shortTag Sell = Invested

{-
extend :: Int -> Vector Int -> Vector Int
extend len vs =
  let l = Vec.last vs
      vs1 = if l < len then Vec.snoc vs len else vs
  in vs1
-}


impulse2tradeListHelp ::
  (Ord t) => (Impulse -> Position) -> Signal t ohlc -> ImpulseSignal stgy -> TradeList stgy t ohlc
impulse2tradeListHelp tag (Signal ps) (ImpulseSignal is) =
  let idxs = Map.toList is
      f (Index i0, imp) (Index i1, _) = Trade (tag imp) (Vec.slice i0 (i1-i0+1) ps)
  in TradeList (zipWith f idxs (tail idxs))


instance Impulse2TradeList Long where
  impulse2tradeList = impulse2tradeListHelp longTag

instance Impulse2TradeList Short where
  impulse2tradeList = impulse2tradeListHelp shortTag 

