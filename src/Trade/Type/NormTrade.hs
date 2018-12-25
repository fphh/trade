{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Type.NormTrade where

import Data.Vector (Vector)

import Trade.Type.Bars (DeltaT)

import Trade.Type.Position (Position)

data NormTrade yield t = NormTrade {
  normTradePosition :: Position
  , normTradeDuration :: DeltaT t
  , normedYield :: Vector yield
  }

deriving instance (Show (DeltaT t), Show yield) => Show (NormTrade yield t)

newtype NormTradeList yield t = NormTradeList {
  unNormTradeList :: [NormTrade yield t]
  } -- deriving (Show)
