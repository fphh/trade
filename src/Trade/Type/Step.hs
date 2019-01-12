{-# LANGUAGE FlexibleContexts #-}

module Trade.Type.Step where

import Trade.Type.Bars (DeltaTy)

import Trade.Type.Step.Commission (Commission(..))
import Trade.Type.Step.Fraction (Fraction(..))
import Trade.Type.Step.Interests (Interests(..), interests)

data Step t = Step {
  fraction :: Fraction
  , commission :: Commission
  , shortInterests :: Maybe (Interests t)
  }



step :: (Real (DeltaTy t)) => Step t
step = Step {
  fraction = Fraction 0.5
  , commission = Commission (\c -> 0.1*c)
  , shortInterests = Just (Interests (interests 0.05))
  }
