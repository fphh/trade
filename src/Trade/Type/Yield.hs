{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Yield where

import Text.Printf (printf)

import Data.Time.Clock (NominalDiffTime)

import Trade.Type.Bars (BarLength, barLength2diffTime)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))

import Trade.Report.Pretty (Pretty, pretty)



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


instance (Pretty t, Pretty a) => Pretty (LogYield t a) where
  pretty (LogYield dt a) = printf "log y = %.8f / %s" a (pretty dt)
  
instance (Pretty t, Pretty a) => Pretty (Yield t a) where
  pretty (Yield dt a) = printf "y = %.8f / %s" a (pretty dt)


yieldPerBar :: BarLength -> LogYield NominalDiffTime a -> LogYield NominalDiffTime a
yieldPerBar bl (LogYield dt y) =
  let i = dt / barLength2diffTime bl
  in LogYield i (y / realToFrac i)
