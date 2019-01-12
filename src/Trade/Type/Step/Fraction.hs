

module Trade.Type.Step.Fraction where

-- | Fraction of your equity you want to trade.
newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)


fullFrac :: Fraction
fullFrac = Fraction 1.0

inverse :: Fraction -> Fraction
inverse (Fraction x) =
  case 0 <= x && x <= 1 of
    True -> Fraction (1-x)
    False -> error "Fraction.inverse: not between 0 and 1"
