{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Yield where

import Text.Printf (printf)

import Data.Time.Clock (NominalDiffTime)

import Trade.Type.Bars (BarLength, barLength2diffTime)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))

import Trade.Report.Pretty (Pretty, pretty)



data Yield a = Yield {
  total :: NominalDiffTime
  , yield :: Double
  } deriving (Show)

data LogYield a = LogYield {
  logDuration :: NominalDiffTime
  , logYield :: Double
  } deriving (Show)

logYield2yield :: LogYield a -> Yield a
logYield2yield (LogYield dt y) = Yield dt (exp y)

yield2logYield :: Yield a -> LogYield a
yield2logYield (Yield dt y) = LogYield dt (log y)

class ToYield a where
  toYield :: NominalDiffTime -> a -> a -> LogYield a

instance ToYield Price where
  toYield dt (Price a) (Price b) = LogYield dt (log (a/b))

instance ToYield Equity where
  toYield dt (Equity a) (Equity b) = LogYield dt (log (a/b))

instance Semigroup (Yield a) where
  (Yield dt a) <> (Yield ds b) = Yield (dt+ds) (a*b)

instance Semigroup (LogYield a) where
  (LogYield dt a) <> (LogYield ds b) = LogYield (dt+ds) (a+b)


instance (Pretty a) => Pretty (LogYield a) where
  pretty (LogYield dt a) = printf "log y = %.8f / %s" a (pretty dt)
  
instance (Pretty a) => Pretty (Yield a) where
  pretty (Yield dt a) = printf "y = %.8f / %s" a (pretty dt)


yieldPerBar :: BarLength -> LogYield a -> LogYield a
yieldPerBar bl (LogYield dt y) =
  let i = dt / barLength2diffTime bl
  in LogYield i (y / realToFrac i)
