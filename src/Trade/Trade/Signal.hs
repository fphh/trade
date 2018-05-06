
module Trade.Trade.State where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing, catMaybes)

import Trade.Trade.Impulse

data Impulse = Buy | Sell deriving (Show, Eq)

newtype ImpulseSignal ohcl = ImpulseSignal (Vector (UTCTime, Maybe Impulse)) deriving (Show)


toImpulseSignal ::
  (a -> Maybe Impulse)
  -> Vector (UTCTime, a)
  -> ImpulseSignal ohcl
toImpulseSignal f vs = ImpulseSignal (Vec.map (fmap f) vs)

buyAndHold :: Vector (UTCTime, ohlc) -> ImpulseSignal ohcl
buyAndHold cs =
  let h = fmap (\_ -> Just Buy) (Vec.head cs)
      l = fmap (\_ -> Just Sell) (Vec.last cs)
      ms = Vec.map (fmap (\_ -> Nothing)) (Vec.init (Vec.tail cs))
  in ImpulseSignal (h `Vec.cons` ms `Vec.snoc` l)

data State =
  Long
  -- | Short
  | NoPosition
  deriving (Show)

data StateSignal ohlc = StateSignal {
  start :: UTCTime
  , offset :: NominalDiffTime
  , signal :: Vector (NominalDiffTime, State)
  } deriving (Show)

impulse2state :: ImpulseSignal ohlc -> StateSignal ohlc
impulse2state (ImpulseSignal is) =
  let (s, _) = Vec.head is
      js = Vec.fromList (catMaybes (Vec.toList (Vec.filter isNothing (Vec.map sequence is))))
      (o, _) = Vec.head js

      f (t0, Sell) (t1, Buy)  = (t1 `diffUTCTime` t0, NoPosition)
      f (t0, Buy) (t1, Sell)  = (t1 `diffUTCTime` t0, Long)
      f _ _ = error "impulse2state: unexpected impulses"

  in StateSignal s (o `diffUTCTime` s) (Vec.zipWith f js (Vec.tail js))


state2impulse :: StateSignal ohcl -> ImpulseSignal ohcl
state2impulse = error "state2impulse: to be defined"
