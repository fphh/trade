

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
  let f (NormTrade NoPosition _ vs) = Vec.replicate (Vec.length vs + 1) (Left (Yield 1))
      f (NormTrade _ _ vs) = Vec.map Right (Vec.cons (Yield 1) vs)
      -- could be more efficient, if we cons before ?

      g (b, x:xs) =
        let len = Vec.length x + b
        in case len < bs of
             False -> Nothing
             True -> Just (x, (len, xs))
      
      cs = Vec.concat $ List.unfoldr g (offs, map f ntl)
      bars = Vec.generate (Vec.length cs) (BarNo . (offs+))

      p (_, Right _) = True
      p _ = False

      unEither (Right x) = x
      unEither _ = error "offsettedNormTradeList2normHistory: should never be Left"

  in NormHistory (Vec.map (fmap unEither) (Vec.filter p (Vec.zip bars cs)))


normHistory2normEquity ::
  (Equity -> Yield -> Equity) -> Equity -> NormHistory ohlc -> NormEquityHistory ohlc
normHistory2normEquity step eqty (NormHistory nhs) =
  let f (_, e) (t1, y) = (t1, step e y)
      (t0, y0) = shead "normHistory2normEquity" nhs      
  in NormEquityHistory (Vec.scanl' f (t0, step eqty y0) (stail "normHistory2normEquity" nhs))
