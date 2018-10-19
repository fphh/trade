
module Trade.Type.OffsettedNormTradeList where


import Trade.Type.Bars (Bars(..))
import Trade.Type.NormTrade (NormTradeList(..))

data OffsettedNormTradeList t = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList t
  } -- deriving (Show)

