
module Trade.Type.OffsettedTradeYieldList where


import Trade.Type.Bars (DeltaTy, BarNo)
import Trade.Type.TradeYield (TradeYieldList(..))

data OffsettedTradeYieldList = OffsettedTradeYieldList {
  offset :: DeltaTy BarNo
  , tradeList :: TradeYieldList
  } deriving (Show)
