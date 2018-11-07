

module Trade.Type.Conversion.Impulse2Position where

import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Position (Position(..))

impulse2position :: Impulse -> Position
impulse2position Buy = Long
impulse2position Sell = NoPosition

      
