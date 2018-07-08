
module Trade.Analysis.OffsettedNormTradeList where

import qualified Data.List as List

import qualified Data.Vector as Vec

import Trade.Trade.TradeList

import Trade.Type.Yield (Yield)
import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.History (History(..))
import Trade.Type.State (State(..))


data OffsettedNormTradeList ohlc = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList ohlc
  } deriving (Show)


offsettedNormTradeList2normHistory ::
  Bars -> OffsettedNormTradeList ohcl -> History Yield
offsettedNormTradeList2normHistory (Bars bs) (OffsettedNormTradeList (Bars offs) (NormTradeList ntl)) =
  let f (o, NormTrade state _ vs : xs) =
        let len = o + Vec.length vs
        in case len <= bs of
             True -> Just $ (\v -> (v, (len, xs))) $
               case state of
                 NoPosition -> Vec.empty
                 _ -> Vec.imap (\i x -> (BarNo (o+i), x)) vs
             False -> Nothing
      f _ = error "offsettedNormTradeList2normHistory"

  in History (Vec.concat (List.unfoldr f (offs, ntl)))
