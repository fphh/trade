{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.Yield where

import Trade.Report.Pretty

-- | Yield: new / old equity
newtype Yield = Yield {
  unYield :: Double
  } deriving (Show, Eq, Ord, Num)

instance Pretty Yield where
  pretty = show . unYield

