

module Trade.Analysis.OffsettedNormTradeList where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

import qualified Data.Vector as Vec

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Trade.SafeTail

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Analysis.NormHistory

data OffsettedNormTradeList ohlc = OffsettedNormTradeList {
  offset :: NominalDiffTime
  , tradeList :: NormTradeList ohlc
  } deriving (Show)

offsettedNormTradeList2normHistory ::
  UTCTime -> NominalDiffTime -> OffsettedNormTradeList ohlc -> NormHistory ohlc
offsettedNormTradeList2normHistory begin day (OffsettedNormTradeList off (NormTradeList ntl)) =
  let f (NormTrade NoPosition _ vs) = Vec.replicate (Vec.length vs + 1) (Left (Yield 1))
      f (NormTrade _ _ vs) = Vec.map Right (Vec.cons (Yield 1) vs)

      cs = Vec.concat (map f ntl)
      
      start = off `addUTCTime` begin
      times = Vec.generate (Vec.length cs) (\i -> (fromIntegral i*day) `addUTCTime` start)

      p (_, Right _) = True
      p _ = False

      unEither (Right x) = x
      unEither _ = error "offsettedNormTradeList2normHistory: should never be Left"

  in NormHistory (Vec.map (fmap unEither) (Vec.filter p (Vec.zip times cs)))


normHistory2normEquity ::
  (Equity -> Yield -> Equity) -> Equity -> NormHistory ohlc -> NormEquityHistory ohlc
normHistory2normEquity step eqty (NormHistory nhs) =
  let f (_, e) (t1, y) = (t1, step e y)
      (t0, y0) = shead "normHistory2normEquity" nhs      
  in NormEquityHistory (Vec.scanl' f (t0, step eqty y0) (stail "normHistory2normEquity" nhs))
