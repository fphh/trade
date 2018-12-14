

module Trade.Type.ImpulseSignal where

-- import Control.Monad (join)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Signal (Signal(..))
import Trade.Type.Impulse (Impulse(..))


newtype ImpulseSignal t = ImpulseSignal {
  unImpulseSignal :: Map t Impulse
  }

expandImpulseSignal :: (Ord t) => Signal t ohlc -> ImpulseSignal t -> Signal t (Maybe Impulse)
expandImpulseSignal (Signal ps) (ImpulseSignal is) =
  let f (t, _) = (t, Map.lookup t is)
  in Signal (Vec.map f ps)


-- | Allow only alternating Buy/Sell
simplify :: (Ord t) => ImpulseSignal t -> ImpulseSignal t
simplify (ImpulseSignal m) =
  let g (Nothing, o) k a = (Just a, Map.insert k a o)
      g acc@(Just x, _) _ a | a == x = acc
      g (Just x, o) k a | a /= x = (Just a, Map.insert k a o)
      g _ _ _ = error "ImpulseSignal.simplify"
  in ImpulseSignal (snd (Map.foldlWithKey' g (Nothing, Map.empty) m))

{-
curve ::
  (Ord t, Num y) =>
  Signal t ohlc -> ImpulseSignal t -> Vector (t, y)
curve (Signal ps) (ImpulseSignal is) =
  let f (t, p) = Vec.fromList $
        (t, 0) : case Map.lookup t is of
                   Just Buy -> [(t, -1), (t, 0)]
                   Just Sell -> [(t, 1), (t, 0)]
                   Nothing -> []
  in join (Vec.map f ps)
-}

curve ::
  (Ord t) =>
  Signal t ohlc -> ImpulseSignal t -> Vector (t, Maybe Impulse)
curve (Signal ps) (ImpulseSignal is) =
  let f t s  = (++[(t, Nothing), (t, Just s), (t, Nothing)])
      (t0, _) = Vec.head ps
      (tn, _) = Vec.last ps
  in Vec.snoc (Vec.fromList (Map.foldrWithKey' f [(t0, Nothing)] is)) (tn, Nothing)
