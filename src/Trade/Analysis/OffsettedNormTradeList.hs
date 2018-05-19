

module Trade.Analysis.OffsettedNormTradeList where

import Control.Applicative (liftA2)

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Trade.TradeList
import Trade.Trade.State
import Trade.Type.Yield

import Trade.Analysis.NormHistory

data OffsettedNormTradeList ohlc = OffsettedNormTradeList {
  offset :: NominalDiffTime
  , tradeList :: NormTradeList ohlc
  } deriving (Show)



offsettedNormTradeList2normHistory ::
  UTCTime -> NominalDiffTime -> OffsettedNormTradeList ohlc -> NormHistory ohlc
offsettedNormTradeList2normHistory begin day (OffsettedNormTradeList off (NormTradeList ntl)) =
  let f (NormTrade Long _ vs) = vs
      f (NormTrade NoPosition _ vs) = Vec.map (const 1) vs
      tkr = Vec.concat (map f ntl)
      
      start = off `addUTCTime` begin

      times = Vec.generate (Vec.length tkr) (\i -> (fromIntegral i*day) `addUTCTime` start)

  in NormHistory (Vec.zip times tkr)

normHistory2normEquity :: NormHistory ohlc -> NormEquityHistory ohlc
normHistory2normEquity (NormHistory nhs) =
  let f (t0, a) (t1, b) = (t1, a*b)
  in NormEquityHistory (Vec.scanl1' f nhs)
