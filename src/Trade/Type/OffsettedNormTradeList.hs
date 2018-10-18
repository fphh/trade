
module Trade.Type.OffsettedNormTradeList where


import Trade.Type.Bars (Bars(..))
import Trade.Type.NormTrade (NormTradeList(..))

data OffsettedNormTradeList = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList
  } deriving (Show)

