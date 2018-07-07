
module Trade.Analysis.OffsettedNormTradeList where

import qualified Data.List as List

-- import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

import qualified Data.Vector as Vec

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Trade.SafeTail

import Trade.Type.Yield (Yield)
import Trade.Type.Equity (Equity)
import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.History

import Trade.Analysis.StepFunc

import Debug.Trace

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

  in History (Vec.concat (List.unfoldr f (offs, ntl)))

normHistory2normEquity ::
  StepFunc -> Equity -> History Yield -> History Equity
normHistory2normEquity step eqty (History nhs) =
  let f (_, e) (t1, y) = (t1, step e y)
      (t0, y0) = shead "normHistory2normEquity" nhs      
  in History (Vec.scanl' f (t0, step eqty y0) (stail "normHistory2normEquity" nhs))
