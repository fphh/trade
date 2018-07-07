
module Trade.Type.Bars where

-- | Duration.
newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord)
