{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Type.Signal.Impulse where

import Control.Monad (join)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Signal (Signal(..))

-- import qualified Trade.Report.Report as Report

import Trade.Report.Curve


type ImpulseSignal t = Signal t (Maybe Impulse)


instance Curve (ImpulseSignal t) where
  type CurveTy (ImpulseSignal t) = t
  
  curve (Signal is) =
    let f (t, Just Sell) = Vec.fromList [(t, 0), (t, 1), (t, 0)]
        f (t, Just Buy) = Vec.fromList [(t, 0), (t, -1), (t, 0)]
        f _ = Vec.empty
    in join (Vec.map f is)


toImpulseSignal ::
  (Int -> t -> evt -> Maybe Impulse)
  -> Vector (t, evt)
  -> ImpulseSignal t
toImpulseSignal f vs = Signal (Vec.imap (\i (t, x) -> (t, f i t x)) vs)

bhImpulse :: Int -> (Int -> t -> evt -> Maybe Impulse)
bhImpulse _ i _ _ | i == 0 = Just Buy
bhImpulse len i _ _ | i == len-1 = Just Sell
bhImpulse _ _ _ _ = Nothing

bhImpulseSignal :: Vector (t, evt) -> ImpulseSignal t
bhImpulseSignal vs = toImpulseSignal (bhImpulse (Vec.length vs - 1)) vs


-- | TODO: This is not very correct!
simplifyImpulseSignal :: ImpulseSignal t -> ImpulseSignal t
simplifyImpulseSignal _ = error "Trade.Type.Signal.Impulse.simplifyImpulseSignal is not very correct"
simplifyImpulseSignal (Signal is) =
  let js = Vec.toList is
      cs = List.groupBy (\x y -> snd x == snd y) js
      ss = map head cs
  in Signal (Vec.fromList ss)
