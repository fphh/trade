
module Trade.Analysis.Bars where


newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show)
