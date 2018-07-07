
module Trade.Analysis.NormHistory where

import Data.Vector (Vector)

import qualified Data.Vector as Vec

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Trade.Curve

import Trade.Analysis.Bars

newtype NormHistory ohlc = NormHistory {
  unNormHistory :: Vector (BarNo, Yield)
  } deriving (Show, Eq)


instance Curve (NormHistory ohlc) where
  curve (NormHistory nh) = Vec.map (\(BarNo x, Yield y) -> (x, y)) nh


newtype NormEquityHistory ohlc = NormEquityHistory {
  unNormEquityHistory :: Vector (BarNo, Equity)
  } deriving (Show, Eq)


instance Curve (NormEquityHistory ohlc) where
  curve (NormEquityHistory nh) = Vec.map (\(BarNo x, Equity y) -> (x, y)) nh

