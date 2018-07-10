

module Trade.Type.Signal where

import Data.Vector (Vector)

import Trade.Report.NumberedList
import Trade.Report.Pretty



newtype Signal t x = Signal {
  unSignal :: Vector (t, x)
  } deriving (Show)

instance (Pretty x, Pretty t) => ToNumberedList (Signal t x) where
  toNumberedList (Signal pps) = toNumberedList pps



data OffsettedSignal t x = OffsettedSignal {
  offset :: t
  , signal :: Signal t x
  } deriving (Show)
