
module Trade.Type.OffsettedNormTradeList where


import Trade.Type.Bars (Bars(..))
import Trade.Type.NormTrade (NormTradeList(..))

data OffsettedNormTradeList yield t = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList yield t
  } -- deriving (Show)

