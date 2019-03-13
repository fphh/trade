{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Step.Interests where


import Trade.Type.Bars (DeltaTy)
import Trade.Type.Equity (Equity(..))


newtype Interests t = Interests {
  unInterests :: Equity -> DeltaTy t -> Double
  }

interests :: (DeltaTy t -> Double) -> Double -> Equity -> DeltaTy t -> Double
interests interestPeriod is (Equity eqty) dt =
  let i' = 1+is
  in eqty*(i' ** (interestPeriod dt)) - eqty
