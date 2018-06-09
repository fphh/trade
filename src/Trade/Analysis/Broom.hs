
module Trade.Analysis.Broom where

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Control.Monad (replicateM)

import Data.Time.Clock (UTCTime)

import Trade.Trade.TradeList
import Trade.Analysis.NormHistory
import Trade.Analysis.MonteCarlo
import Trade.Analysis.OffsettedNormTradeList

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Trade.Curve
import qualified Trade.Report.Report as Report

newtype Broom history = Broom {
  unBroom :: [history]
  } deriving (Show)


broom2chart :: (Curve history) => Int -> Broom history -> [Report.LineTy UTCTime Double]
broom2chart n (Broom xs) =
  let f i x = Report.line (show i) (curve x)
  in zipWith f [0 :: Integer ..] (take n xs)



normHistoryBroom :: Int -> UTCTime -> UTCTime -> NormTradeList ohlc -> IO (Broom (NormHistory ohlc))
normHistoryBroom n begin end ntl = do
  tls <- replicateM n (randomYieldSignal begin end ntl)
  let day = 24*60*60
  return (Broom (map (offsettedNormTradeList2normHistory begin day) tls))


normEquityBroom ::
  (Equity -> Yield -> Equity) -> Equity -> Broom (NormHistory ohlc) -> Broom (NormEquityHistory ohlc)
normEquityBroom step eqty (Broom bs) = Broom (map (normHistory2normEquity step eqty) bs)


type Percent = Double

-- twr = terminal wealth relative
terminalWealthRelative :: Equity -> Broom (NormEquityHistory ohlc) -> Vector (Percent, Double)
terminalWealthRelative (Equity e) (Broom hs) =
  let twrs = List.sort (map ((/e) . unEquity . snd . Vec.last . unNormEquityHistory) hs)
      len = fromIntegral (length twrs)
      g i w = (i/len, w)
  in Vec.fromList (zipWith g [0..] twrs)

risk :: Broom (NormEquityHistory ohlc) -> Vector (Percent, Double)
risk (Broom hs) =
  let f vs =
        let l = Vec.length vs
            g i x =
              let us = Vec.filter (<=x) (Vec.slice i (l-i) vs)
              in case Vec.length us of
                   0 -> 1/0
                   _ -> Vec.maximum (Vec.map ((1-) . (/x)) us)

        in Vec.maximum (Vec.imap g vs)
        
      len = fromIntegral (length hs)

      
      qs = map (f . Vec.map (unEquity . snd) . unNormEquityHistory) hs

      h i w = (i/len, w)

  in Vec.fromList (zipWith h [0..] (List.sort qs))
