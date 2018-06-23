

module Trade.Trade.Curve where

import Data.Time.Clock (UTCTime)

import Data.Vector (Vector)

class Curve a where
  curve :: a -> Vector (Int, Double)
