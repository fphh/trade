

module Trade.Type.Step.Algorithm where


import qualified Data.Vector as Vec

import Trade.Type.Bars (Add, add)

import Trade.Type.Delta (Delta(..))
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step (Step(..))
import Trade.Type.Step.Commission (Commission(..))
import Trade.Type.Step.Fraction (Fraction(..))
import Trade.Type.Step.Interests (Interests(..))

import Trade.Help.SafeTail (slast)



stepFunction :: (Add t) => Step t -> Equity -> DeltaSignal t ohlc -> Signal t Equity
stepFunction step (Equity eqty) (DeltaSignal t LongPosition (Signal as)) =
  let Commission com = commission step
      Fraction frac = fraction step
      e0 = frac * eqty
      e1 = ((1-frac) * eqty) - com e0
      f (dt, Delta dy) = (dt `add` t, Equity ((dy * abs e0) + e0 + e1))
      g (Equity e) = Equity (e - com (e-e1))
  in Signal.mapLast g (Signal (Vec.map f as))

stepFunction _ eqty (DeltaSignal t NoPosition as) =
  let f (dt, _) = (dt `add` t, eqty)
  in Signal.map f as

stepFunction step (Equity eqty) (DeltaSignal t ShortPosition (Signal as)) =
  let Commission com = commission step
      Fraction frac = fraction step
      
      e0 = frac * eqty
      e1 = eqty - com e0

      (dtn, _) = slast "stepFunction: slast" as
      f (dt, Delta dy) = (dt `add` t, Equity ((dy * abs e0) + e0 + e1))

      si = maybe 0 (\(Interests inters) -> inters (Equity e0) dtn) (shortInterests step)
      
      g (Equity e) = Equity (e - com (e-e1) - si)
  in Signal.mapLast g (Signal (Vec.map f as))
