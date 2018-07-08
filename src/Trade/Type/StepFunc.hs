

module Trade.Type.StepFunc where

import Trade.Type.Fraction (Fraction(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Yield (Yield(..))
import Trade.Type.Commission (Commission(..))


-- | StepFunc takes a yield and an equity and returns the equity with the yield applied.
type StepFunc = Equity -> Yield -> Equity


-- | Genearal function which keeps track of commissions payed to the broker and
-- | uses only a fraction of your total equity.
stepFunc :: Commission -> Fraction -> StepFunc
stepFunc (Commission com) (Fraction frac) (Equity e) (Yield y) =
  let e0 = frac * e
      e1 = (1-frac) * e
  in Equity (e1 + com (e0*y))


stepFuncNoCommission :: Fraction -> StepFunc
stepFuncNoCommission = stepFunc (Commission id)

stepFuncNoCommissionFullFraction :: StepFunc
stepFuncNoCommissionFullFraction = stepFunc (Commission id) (Fraction 1)

stepFuncFixedPrice :: Double -> Fraction -> StepFunc
stepFuncFixedPrice price = stepFunc (Commission (\x -> x-price))

stepFuncRelativePrice :: Double -> Fraction -> StepFunc
stepFuncRelativePrice q = stepFunc (Commission (\x -> (1-q)*x))
