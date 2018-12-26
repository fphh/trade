
module Trade.Type.Conversion.OffsettedTradeYieldList2NormSignal where

import qualified Data.List as List

import qualified Data.Vector as Vec

import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.TradeYield (TradeYield(..), TradeYieldList(..))
import Trade.Type.OffsettedTradeYieldList (OffsettedTradeYieldList(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Position (Position(..))
import Trade.Type.Yield (Yield(..))


offsettedTradeYieldList2normSignal ::
  Bars -> OffsettedTradeYieldList -> Signal BarNo Yield
offsettedTradeYieldList2normSignal (Bars bs) (OffsettedTradeYieldList (Bars offs) (TradeYieldList ntl)) =
  let f (o, TradeYield position vs : xs) =
        let len = o + Vec.length vs
        in case len <= bs of
             True -> Just $ (\v -> (v, (len, xs))) $
               case position of
                 NoPosition -> Vec.empty
                 _ -> Vec.imap (\i x -> (BarNo (o+i), x)) vs
             False -> Nothing
      f _ = error "offsettedTradeYieldList2normSignal"

  in Signal (Vec.concat (List.unfoldr f (offs, ntl)))
