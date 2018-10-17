
module Trade.Type.OffsettedNormTradeList where

import qualified Data.List as List

import qualified Data.Vector as Vec

import Trade.Type.Yield (Yield)
import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.History (History(..))
import Trade.Type.State (State(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))


data OffsettedNormTradeList = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList
  } deriving (Show)

