{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Type.Signal.Impulse where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import Data.Time.Clock (UTCTime)

import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Signal (Signal(..))

-- import qualified Trade.Report.Report as Report

import Trade.Report.Curve


type ImpulseSignal = Signal UTCTime (Maybe Impulse)


instance Curve ImpulseSignal where
  type Ty ImpulseSignal = UTCTime
  
  curve (Signal is) =
    let f (t, Just Sell) = [(t, 0), (t, 1), (t, 0)]
        f (t, Just Buy) = [(t, 0), (t, -1), (t, 0)]
        f _ = []
    in head (sequence (Vec.map f is))



-- impulse2line :: ImpulseArgs -> ImpulseSignal -> Report.LineTyR UTCTime z Double
-- impulse2line args imps = Report.lineR "buy/sell" (impulse2line' args imps)



toImpulseSignal ::
  (Int -> UTCTime -> evt -> Maybe Impulse)
  -> Vector (UTCTime, evt)
  -> ImpulseSignal
toImpulseSignal f vs = Signal (Vec.imap (\i (t, x) -> (t, f i t x)) vs)

bhImpulse :: Int -> (Int -> UTCTime -> evt -> Maybe Impulse)
bhImpulse _ i _ _ | i == 0 = Just Buy
bhImpulse len i _ _ | i == len-1 = Just Sell
bhImpulse _ _ _ _ = Nothing

bhImpulseSignal :: Vector (UTCTime, evt) -> ImpulseSignal
bhImpulseSignal vs = toImpulseSignal (bhImpulse (Vec.length vs - 1)) vs

simplifyImpulseSignal :: ImpulseSignal -> ImpulseSignal
simplifyImpulseSignal (Signal is) =
  let js = Vec.toList is
      cs = List.groupBy (\x y -> snd x == snd y) js
      ss = map head cs
  in Signal (Vec.fromList ss)
