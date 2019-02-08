

module Trade.Type.NonEmptyList where

-- | Non empty list. We should replace it by some package.
data NonEmptyList a = NonEmptyList {
  head :: a
  , tail :: [a]
  }
