

module Trade.Type.Conversion.Impulse2Position where

import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Position (Position(..))
import Trade.Type.Strategy (Strategy(..))

impulse2position :: Strategy -> Impulse -> Position
impulse2position Long Buy = LongPosition
impulse2position Long Sell = NoPosition
impulse2position Short Buy = NoPosition
impulse2position Short Sell = ShortPosition

      
