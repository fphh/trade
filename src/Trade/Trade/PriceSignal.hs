
module Trade.Trade.PriceSignal where

import Data.Time.Clock (UTCTime)

import Data.Vector (Vector)

import Trade.Report.NumberedList
import Trade.Report.Pretty


newtype PriceSignal ohcl = PriceSignal {
  unPriceSignal :: Vector (UTCTime, ohcl)
  } deriving (Show)

instance (Pretty ohlc) => ToNumberedList (PriceSignal ohlc) where
  toNumberedList (PriceSignal pps) = toNumberedList pps

