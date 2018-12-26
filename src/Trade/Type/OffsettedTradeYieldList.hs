
module Trade.Type.OffsettedTradeYieldList where


import Trade.Type.Bars (Bars(..))
import Trade.Type.TradeYield (TradeYieldList(..))

data OffsettedTradeYieldList = OffsettedTradeYieldList {
  offset :: Bars
  , tradeList :: TradeYieldList
  } deriving (Show)

