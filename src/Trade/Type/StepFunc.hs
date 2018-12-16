

module Trade.Type.StepFunc where

import Trade.Type.Fraction (Fraction(..), fullFrac)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Yield (Yield(..), LogYield(..))
import Trade.Type.Commission (Commission(..), noCommission)


-- | StepFunc takes a yield and an equity and returns the equity with the yield applied.
type StepFunc yield = Equity -> yield -> Equity

-- | Genearal function which pays commissions to the broker and
-- uses only a fraction of your total equity.
class StepFunction yield where
  stepFunction :: Commission -> Fraction -> StepFunc yield

instance StepFunction Yield where
  stepFunction (Commission com) (Fraction frac) (Equity e) (Yield y) =
    let e0 = frac * e            -- Traded fraction
        e1 = (1-frac) * e        -- Non traded fraction
    in Equity (com e1 + e0 * y)  -- Deducing fees from non traded fraction

instance StepFunction LogYield where
  stepFunction (Commission com) (Fraction frac) (Equity e) (LogYield y) =
    let e0 = frac * e
        e1 = (1-frac) * e
    in Equity (com e1 + e0 * exp y)



stepFuncNoCommission :: (StepFunction yield) => Fraction -> StepFunc yield
stepFuncNoCommission = stepFunction noCommission

stepFuncNoCommissionFullFraction :: (StepFunction yield) => StepFunc yield
stepFuncNoCommissionFullFraction = stepFunction noCommission fullFrac

stepFuncFixedPrice :: (StepFunction yield) => Double -> Fraction -> StepFunc yield
stepFuncFixedPrice price = stepFunction (Commission (\x -> x-price))

stepFuncRelativePrice ::(StepFunction yield) =>  Double -> Fraction -> StepFunc yield
stepFuncRelativePrice q = stepFunction (Commission (\x -> (1-q)*x))

