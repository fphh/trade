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



-- | TODO: Verfy this!
alternateBuySellKeepFirstOccurrence :: ImpulseSignal t -> ImpulseSignal t
alternateBuySellKeepFirstOccurrence (Signal is) =
  let (ts, ds) = unzip (Vec.toList is)

      go xs =
        let p a x = x == a || x == Nothing
            toNothing = map (const Nothing)
            go a xs = (a:) $
              case List.span (p a) xs of
                (as, []) -> toNothing as
                (as, [b]) -> toNothing as ++ [b]
                (as, b:bs) -> toNothing as ++ go b bs
        in case xs of
          [] -> error "alternateBuySellKeepFirstOccurrence: signal shorter than 1"
          u:us -> go u us

      es = go ds
      
   in Signal (Vec.fromList (zip ts es))

{-
adjustSignal :: ImpulseSignal t -> ImpulseSignal t
adjustSignal (Signal is) =
  let len = Vec.length is
      (a, as) = Vec.splitAt 1 is
      (os, o) = Vec.splitAt (len-1) is
      oe = Vec.head o

      newAe = case Vec.head a of
  -}              
