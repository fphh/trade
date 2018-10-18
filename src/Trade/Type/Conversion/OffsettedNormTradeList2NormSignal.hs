
module Trade.Type.Conversion.OffsettedNormTradeList2NormSignal where

import qualified Data.List as List

import qualified Data.Vector as Vec

import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))
import Trade.Type.OffsettedNormTradeList (OffsettedNormTradeList(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.State (State(..))
import Trade.Type.Yield (Yield(..))

offsettedNormTradeList2normSignal ::
  Bars -> OffsettedNormTradeList -> Signal BarNo Yield
offsettedNormTradeList2normSignal (Bars bs) (OffsettedNormTradeList (Bars offs) (NormTradeList ntl)) =
  let f (o, NormTrade state _ vs : xs) =
        let len = o + Vec.length vs
        in case len <= bs of
             True -> Just $ (\v -> (v, (len, xs))) $
               case state of
                 NoPosition -> Vec.empty
                 _ -> Vec.imap (\i x -> (BarNo (o+i), x)) vs
             False -> Nothing
      f _ = error "offsettedNormTradeList2normSignal"

  in Signal (Vec.concat (List.unfoldr f (offs, ntl)))
