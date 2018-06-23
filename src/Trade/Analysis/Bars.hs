
module Trade.Analysis.Bars where


newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)
