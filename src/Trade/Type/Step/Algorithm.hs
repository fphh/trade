{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trade.Type.Step.Algorithm where


import qualified Data.Vector as Vec

import Trade.Type.Bars (Add, add)

import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(Invested, NotInvested))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step (StepTy, longCommission, longFraction, shortCommission, shortFraction, shortInterests)
import Trade.Type.Strategy (Long, Short)

import Trade.Type.Step.Commission (Commission(..))
import Trade.Type.Step.Fraction (Fraction(..))
import Trade.Type.Step.Interests (Interests(..))

import Trade.Help.SafeTail (slast)


class (Add t) => StepFunction step t where
  stepFunction :: step t -> Equity -> DeltaSignal t ohlc -> Signal t Equity

instance (Add t) => StepFunction (StepTy Long) t where
  stepFunction step (Equity eqty) (DeltaSignal t Invested (Signal as)) =
    let Commission com = longCommission step
        Fraction frac = longFraction step
        e0 = frac * eqty
        e1 = ((1-frac) * eqty) - com e0
        f (dt, Delta dy) = (dt `add` t, Equity ((dy * abs e0) + e0 + e1))
        g (Equity e) = Equity (e - com (abs (e-e1)))
    in Signal.mapLast g (Signal (Vec.map f as))

  stepFunction _ eqty (DeltaSignal t NotInvested as) =
    let f (dt, _) = (dt `add` t, eqty)
    in Signal.map f as


instance (Add t) => StepFunction (StepTy Short) t where

  stepFunction step (Equity eqty) (DeltaSignal t Invested (Signal as)) =
    let Commission com = shortCommission step
        Fraction frac = shortFraction step
      
        e0 = frac * abs eqty
        e1 = eqty - com e0

        (dtn, _) = slast "stepFunction: slast" as
        f (dt, Delta dy) = (dt `add` t, Equity ((dy * e0) + e0 + e1))

        si = unInterests (shortInterests step) (Equity e0) dtn
        
        g (Equity e) = Equity (e - e0 - (com (abs (e-e1))) - si)
    in Signal.mapLast g (Signal (Vec.map f as))


  stepFunction _ eqty (DeltaSignal t NotInvested as) =
    let f (dt, _) = (dt `add` t, eqty)
    in Signal.map f as
