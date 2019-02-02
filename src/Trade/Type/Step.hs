{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Step where


import Trade.Type.Step.Commission (Commission(..))
import Trade.Type.Step.Fraction (Fraction(..))
import Trade.Type.Step.Interests (Interests) -- (..), interests)


import Trade.Type.Strategy (Short, Long)


data family StepTy a :: * -> *


data instance StepTy Long t = LongStep {
  longFraction :: Fraction
  , longCommission :: Commission
  }


data instance StepTy Short t = ShortStep {
  shortFraction :: Fraction
  , shortCommission :: Commission
  , shortInterests :: Interests t
  }
