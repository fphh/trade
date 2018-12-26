
module Trade.Type.TradeYield where

import Data.Vector (Vector)

import Trade.Type.Position (Position)
import Trade.Type.Yield (Yield)

data TradeYield = TradeYield {
  tradeYieldsPosition :: Position
  , tradeYields :: Vector Yield
  } deriving (Show)

newtype TradeYieldList = TradeYieldList {
  unTradeYieldList :: [TradeYield]
  } deriving (Show)
