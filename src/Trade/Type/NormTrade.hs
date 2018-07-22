

module Trade.Type.NormTrade where

import Data.Vector (Vector)

import Data.Time.Clock (NominalDiffTime)

import Trade.Type.State (State)
import Trade.Type.Yield (Yield)


data NormTrade = NormTrade {
  normTradeState :: State
  , normTradeDuration :: NominalDiffTime
  , normedYield :: Vector Yield
  } deriving (Show)


newtype NormTradeList = NormTradeList {
  unNormTradeList :: [NormTrade]
  } deriving (Show)

