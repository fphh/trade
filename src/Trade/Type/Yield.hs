{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Yield where

import Debug.Trace

import Trade.Report.Pretty



-- | Yield: `end / start` equity
newtype Yield = Yield {
  unYield :: Double
  } deriving (Show, Eq, Ord, Num)

instance Pretty Yield where
  pretty = show . unYield


instance Semigroup Yield where
  (Yield x) <> (Yield y) = Yield (x*y)
  
instance Monoid Yield where
  mempty = Yield 1


-- | LogYield: `log (end / start)` equity
newtype LogYield = LogYield {
  unLogYield :: Double
  } deriving (Show, Eq, Ord, Num, Fractional)

instance Pretty LogYield where
  pretty = show . unLogYield


instance Semigroup LogYield where
  (LogYield x) <> (LogYield y) = LogYield (x+y)

instance Monoid LogYield where
  mempty = LogYield 0

class ToYield yield where
  toYield :: Double -> Double -> yield

  yield :: Double -> yield

instance ToYield Yield where
  toYield new old = Yield (new / old)

  yield = Yield

instance ToYield LogYield where
  toYield new old = LogYield (log (new / old))

  yield = LogYield . log


