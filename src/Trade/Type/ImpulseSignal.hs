

module Trade.Type.ImpulseSignal where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Signal (Signal(..))
import Trade.Type.Impulse (Impulse(..))

import Trade.Type.Strategy.Index (Index(..))


newtype ImpulseSignal stgy = ImpulseSignal {
  unImpulseSignal :: Map Index Impulse
  } deriving (Show)


curve ::
  (Ord t) =>
  Vector t -> ImpulseSignal stgy -> Vector (t, Maybe Impulse)
curve ts (ImpulseSignal is) =
  let f (Index idx) s = (++[(ts Vec.! idx, Nothing), (ts Vec.! idx, Just s), (ts Vec.! idx, Nothing)])
      t0 = Vec.head ts
      tn = Vec.last ts
  in Vec.snoc (Vec.fromList (Map.foldrWithKey' f [(t0, Nothing)] is)) (tn, Nothing)


