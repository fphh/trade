{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.History where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Bars (BarNo(..))
import Trade.Type.Yield (Yield(..))
import Trade.Type.Equity (Equity(..))

import Trade.Trade.Curve

-- | History by `BarNo`, usually `Equity` or `Yield`.
newtype History p = History {
  unHistory :: Vector (BarNo, p)
  } deriving (Show, Eq)

instance Functor History where
  fmap f (History h) = History (Vec.map (fmap f) h)

instance Curve (History Yield) where
  curve = fmap (\(BarNo x, Yield y) -> (x, y)) . unHistory

instance Curve (History Equity) where
  curve = fmap (\(BarNo x, Equity y) -> (x, y)) . unHistory

