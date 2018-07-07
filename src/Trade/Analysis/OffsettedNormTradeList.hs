
module Trade.Analysis.OffsettedNormTradeList where

import qualified Data.List as List

-- import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

import qualified Data.Vector as Vec

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Trade.SafeTail

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Analysis.NormHistory
import Trade.Analysis.Bars

import Debug.Trace

data OffsettedNormTradeList ohlc = OffsettedNormTradeList {
  offset :: Bars
  , tradeList :: NormTradeList ohlc
  } deriving (Show)

offsettedNormTradeList2normHistory ::
  Bars -> OffsettedNormTradeList ohlc -> NormHistory ohlc
offsettedNormTradeList2normHistory (Bars bs) (OffsettedNormTradeList (Bars offs) (NormTradeList ntl)) =
  let f (o, NormTrade state _ vs : xs) =
        let len = o + Vec.length vs
        in case len <= bs of
             True -> Just $ (\v -> (v, (len, xs))) $
               case state of
                 NoPosition -> Vec.empty
                 _ -> Vec.imap (\i x -> (BarNo (o+i), x)) vs
             False -> Nothing

  in NormHistory (Vec.concat (List.unfoldr f (offs, ntl)))

normHistory2normEquity ::
  (Equity -> Yield -> Equity) -> Equity -> NormHistory ohlc -> NormEquityHistory ohlc
normHistory2normEquity step eqty (NormHistory nhs) =
  let f (_, e) (t1, y) = (t1, step e y)
      (t0, y0) = shead "normHistory2normEquity" nhs      
  in NormEquityHistory (Vec.scanl' f (t0, step eqty y0) (stail "normHistory2normEquity" nhs))
