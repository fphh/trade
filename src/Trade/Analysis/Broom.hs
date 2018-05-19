
module Trade.Analysis.Broom where

import Control.Monad (replicateM)

import Data.Time.Clock (UTCTime)

import Trade.Trade.TradeList
import Trade.Analysis.NormHistory
import Trade.Analysis.MonteCarlo
import Trade.Analysis.OffsettedNormTradeList

newtype Broom history = Broom {
  unBroom :: [history]
  } deriving (Show)

normHistoryBroom :: Int -> UTCTime -> UTCTime -> NormTradeList ohlc -> IO (Broom (NormHistory ohlc))
normHistoryBroom n begin end ntl = do
  tls <- replicateM n (randomYieldSignal begin end ntl)
  return (Broom (map (offsettedNormTradeList2normHistory begin (24*60*60)) tls))

normEquityBroom :: Broom (NormHistory ohlc) -> Broom (NormEquityHistory ohlc)
normEquityBroom (Broom bs) = Broom (map normHistory2normEquity bs)
