{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Yield where

import Debug.Trace

import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))

import Trade.Report.Pretty



data Yield t a = Yield {
  total :: t
  , yield :: Double
  } deriving (Show)

data LogYield t a = LogYield {
  logDuration :: t
  , logYield :: Double
  } deriving (Show)

logYield2yield :: LogYield t a -> Yield t a
logYield2yield (LogYield dt y) = Yield dt (exp y)

yield2logYield :: Yield t a -> LogYield t a
yield2logYield (Yield dt y) = LogYield dt (log y)


class ToYield a where
  toYield :: t -> a -> a -> LogYield t a

instance ToYield Price where
  toYield dt (Price a) (Price b) = LogYield dt (log (a/b))

instance ToYield Equity where
  toYield dt (Equity a) (Equity b) = LogYield dt (log (a/b))

instance (Num t) => Semigroup (Yield t a) where
  (Yield dt a) <> (Yield ds b) = Yield (dt+ds) (a*b)

instance (Num t) => Semigroup (LogYield t a) where
  (LogYield dt a) <> (LogYield ds b) = LogYield (dt+ds) (a+b)



{-

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


-}
