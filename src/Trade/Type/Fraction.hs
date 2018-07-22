

module Trade.Type.Fraction where

-- | Fraction of your equity you want to trade.
newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)


fullFrac :: Fraction
fullFrac = Fraction 1.0
