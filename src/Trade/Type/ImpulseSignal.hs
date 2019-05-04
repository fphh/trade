

module Trade.Type.ImpulseSignal where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Signal (Signal(..))
import Trade.Type.Impulse (Impulse(..))
import qualified Trade.Type.Impulse as Imp


newtype ImpulseSignal stgy t = ImpulseSignal {
  unImpulseSignal :: Map t Impulse
  } deriving (Show)

expandImpulseSignal :: (Ord t) => Signal t ohlc -> ImpulseSignal stgy t -> Signal t (Maybe Impulse)
expandImpulseSignal (Signal ps) (ImpulseSignal is) =
  let f (t, _) = (t, Map.lookup t is)
  in Signal (Vec.map f ps)


-- | Allow only alternating Buy/Sell
simplify :: (Ord t) => ImpulseSignal stgy t -> ImpulseSignal stgy t
simplify (ImpulseSignal m) =
  let g (Nothing, o) k a = (Just a, Map.insert k a o)
      g acc@(Just x, _) _ a | a == x = acc
      g (Just x, o) k a | a /= x = (Just a, Map.insert k a o)
      g _ _ _ = error "ImpulseSignal.simplify"
  in ImpulseSignal (snd (Map.foldlWithKey' g (Nothing, Map.empty) m))

{-
curve ::
  (Ord t) =>
  Signal t ohlc -> ImpulseSignal stgy t -> Vector (t, Maybe Impulse)
curve (Signal ps) (ImpulseSignal is) =
  let f t s  = (++[(t, Nothing), (t, Just s), (t, Nothing)])
      (t0, _) = Vec.head ps
      (tn, _) = Vec.last ps
  in Vec.snoc (Vec.fromList (Map.foldrWithKey' f [(t0, Nothing)] is)) (tn, Nothing)
-}

curve ::
  (Ord t) =>
  Vector t -> ImpulseSignal stgy t -> Vector (t, Maybe Impulse)
curve ts (ImpulseSignal is) =
  let f t s  = (++[(t, Nothing), (t, Just s), (t, Nothing)])
      t0 = Vec.head ts
      tn = Vec.last ts
  in Vec.snoc (Vec.fromList (Map.foldrWithKey' f [(t0, Nothing)] is)) (tn, Nothing)


invert :: ImpulseSignal stgy t -> ImpulseSignal stgy t
invert (ImpulseSignal m) = ImpulseSignal (fmap Imp.invert m)


