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
