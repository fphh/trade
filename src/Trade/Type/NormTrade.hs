

module Trade.Type.NormTrade where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Bars (DeltaT)

import Trade.Type.Position (Position)
import Trade.Type.Yield (Yield)

data NormTrade t = NormTrade {
  normTradePosition :: Position
  , normTradeDuration :: DeltaT t
  , normedYield :: Vector Yield
  } -- deriving (Show)


newtype NormTradeList t = NormTradeList {
  unNormTradeList :: [NormTrade t]
  } -- deriving (Show)
