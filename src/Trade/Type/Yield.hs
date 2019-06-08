-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Yield where

import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Data.String (fromString)

import Formatting (Format, (%), format, mapf, fixed, int)
import Formatting.Time (datetime, diff, days, minutes, hours, years, decimals)




import Data.Time.Clock (NominalDiffTime)

import Trade.Type.BarLength (Bars(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))

import Trade.Report.Pretty (Pretty, pretty)

import Debug.Trace


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
  pretty (LogYield dt a) =
    Text.unpack (format (fromString "log y = " % fixed 8 % fromString " / ") a) ++ pretty dt


instance (Pretty a) => Pretty (Yield a) where
  pretty (Yield dt a) =
    Text.unpack (format (fromString "y = " % fixed 8 % fromString " / ") a) ++ pretty dt


yieldPerBar :: Bars -> LogYield a -> LogYield a
yieldPerBar (Bars bs) (LogYield dt y) =
  let i = dt / realToFrac bs
  in LogYield i (y / fromIntegral bs)
