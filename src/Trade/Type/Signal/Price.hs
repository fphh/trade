
module Trade.Type.Signal.Price where

import Data.Time.Clock (UTCTime)

import Trade.Type.Signal


type PriceSignal ohcl = Signal UTCTime ohcl

{-

newtype PriceSignal ohcl = PriceSignal {
  unPriceSignal :: Vector (UTCTime, ohcl)
  } deriving (Show, Read)
-}
