
module Trade.Trade.SafeTail where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Debug.Trace

safeTail :: Vector a -> Maybe (Vector a)
safeTail vs =
  case Vec.length vs of
    0 -> Nothing
    _ -> Just (Vec.tail vs)

stail :: String -> Vector a -> Vector a
stail str vs = maybe (error ("safeTail: " ++ str)) id (safeTail vs)

shead :: String -> Vector a -> a
shead str vs = maybe (error ("safeHead: " ++ str)) id (Vec.headM vs)

slast :: String -> Vector a -> a
slast str vs = maybe (error ("safeLast: " ++ str)) id (Vec.lastM vs)
