
module Trade.Analysis.NormHistory where

import Data.Time.Clock (UTCTime)

import Data.Vector (Vector)

import qualified Data.Vector as Vec

import Trade.Type.Yield

import Trade.Trade.Curve

newtype NormHistory ohlc = NormHistory {
  unNormHistory :: Vector (UTCTime, Yield)
  } deriving (Show)

instance Curve (NormHistory ohlc) where
  curve (NormHistory nh) = Vec.map (fmap unYield) nh
  

newtype NormEquityHistory ohlc = NormEquityHistory {
  unNormEquityHistory :: Vector (UTCTime, Yield)
  } deriving (Show)

instance Curve (NormEquityHistory ohlc) where
  curve (NormEquityHistory nh) = Vec.map (fmap unYield) nh
  
