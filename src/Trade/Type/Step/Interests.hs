{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Step.Interests where


import Trade.Type.Bars (DeltaTy)
import Trade.Type.Equity (Equity(..))


newtype Interests t = Interests {
  unInterests :: Equity -> DeltaTy t -> Double
  }

interests :: (Real (DeltaTy t)) => Double -> Equity -> DeltaTy t -> Double
interests i (Equity e) dt =
  let i' = 1+i
      day = 60*60*24
      dt' = realToFrac dt / day
  in e*(i' ** dt') - e
