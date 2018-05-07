{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.Yield where

import Trade.Type.EquityAndShare

newtype Yield = Yield { unYield :: Double } deriving (Show, Eq, Ord, Num)

class ToYield a where
  forwardYield :: a -> a -> Yield

instance ToYield Close where
  forwardYield (Close old) (Close new) = Yield (new / old)


