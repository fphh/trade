

module Trade.Analysis.Yield where


import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.List as List

import Trade.Type.Position (Position)
import Trade.Type.Trade (Trade(..), TradeList(..))

sortTradesByPosition :: TradeList stgy t ohlc -> Map Position (TradeList stgy t ohlc)
sortTradesByPosition (TradeList tl) =
  let f acc t@(Trade stat _) = Map.insertWith (++) stat [t] acc 
  in fmap TradeList (List.foldl' f Map.empty tl)
