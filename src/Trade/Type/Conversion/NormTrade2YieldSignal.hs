{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Conversion.NormTrade2YieldSignal where

import qualified Data.Vector as Vec

import Trade.Type.Bars (Time, DeltaT, add)
import Trade.Type.Position (Position(..))
import Trade.Type.Yield (Yield(..))

import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Yield (YieldSignal)

import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

-- | TODO: check wether correct
yieldAccordingToPosition :: NormTrade t -> NormTrade t
yieldAccordingToPosition (NormTrade NoPosition t vs) =
  NormTrade NoPosition t (Vec.replicate (Vec.length vs {- + 1 -}) (Yield 1))
yieldAccordingToPosition (NormTrade state t vs) =
  NormTrade state t vs -- (Vec.cons (Yield 1) vs)

-- | TODO: check wether correct
normTrade2yieldSignal ::
  (Time t, Num (DeltaT t)) =>
  t -> DeltaT t -> NormTradeList t -> YieldSignal t
normTrade2yieldSignal start dt (NormTradeList nts) =
  let xs = map (normedYield . yieldAccordingToPosition) nts
      f i x = (start `add` (fromIntegral i * dt), x)
      sig = Vec.imap f (Vec.cons (Yield 1) (Vec.concat xs))
  in Signal sig