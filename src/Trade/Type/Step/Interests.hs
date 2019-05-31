

module Trade.Type.Step.Interests where


import Data.Time.Clock (NominalDiffTime)

import Trade.Type.Equity (Equity(..))


newtype Interests = Interests {
  unInterests :: Equity -> NominalDiffTime -> Double
  }

interests :: (dt -> Double) -> Double -> Equity -> dt -> Double
interests interestPeriod is (Equity eqty) dt =
  let i' = 1+is
  in eqty*(i' ** (interestPeriod dt)) - eqty
