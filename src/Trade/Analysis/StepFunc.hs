

module Trade.Analysis.StepFunc where

import Trade.Type.Fraction
import Trade.Type.EquityAndShare
import Trade.Type.Yield

newtype Commission = Commission {
  unCommission :: Double -> Double
  }

type StepFunc = Equity -> Yield -> Equity


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
