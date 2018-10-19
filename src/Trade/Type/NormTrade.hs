{-# LANGUAGE TypeFamilies #-}


module Trade.Type.NormTrade where

import Data.Vector (Vector)

import Trade.Type.Bars (DeltaT)

import Trade.Type.State (State)
import Trade.Type.Yield (Yield)

data NormTrade t = NormTrade {
  normTradeState :: State
  , normTradeDuration :: DeltaT t
  , normedYield :: Vector Yield
  } -- deriving (Show)


newtype NormTradeList t = NormTradeList {
  unNormTradeList :: [NormTrade t]
  } -- deriving (Show)

