
module Trade.Analysis.Broom where

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Control.Monad (replicateM)

import Trade.Type.Yield (Yield(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Fraction (Fraction)
import Trade.Type.Bars (Bars)
import Trade.Type.History (History(..))

import Trade.Trade.TradeList
import qualified Trade.Analysis.MonteCarlo as MC
import Trade.Analysis.OffsettedNormTradeList
import Trade.Analysis.StepFunc

import Trade.Trade.SafeTail

import Trade.Trade.State
import Trade.Trade.Curve
import qualified Trade.Report.Report as Report


newtype Broom history = Broom {
  unBroom :: [history]
  } deriving (Show, Eq)


broom2chart :: (Curve history) => Int -> Broom history -> [Report.LineTy Int Double]
broom2chart n (Broom xs) =
  let f i x = Report.line (show i) (curve x)
  in zipWith f [0 :: Integer ..] (take n xs)

normHistoryBroom :: Bars -> Int -> NormTradeList ohlc -> IO (Broom (History Yield))
normHistoryBroom bs n ntl = do
  let soffs = MC.startingOffsets ntl
      f (NormTrade NoPosition t vs) =
        NormTrade NoPosition t (Vec.replicate (Vec.length vs + 1) (Yield 1))
      f (NormTrade state t vs) =
        NormTrade state t (Vec.cons (Yield 1) vs)

      ntl' = NormTradeList (map f (unNormTradeList ntl))
      
  offsTls <- replicateM n (MC.randomYieldSignal ntl' soffs)

  return (Broom (map (offsettedNormTradeList2normHistory bs) offsTls))


normEquityBroom ::
  StepFunc -> Equity -> Broom (History Yield) -> Broom (History Equity)
normEquityBroom step eqty (Broom bs) = Broom (map (normHistory2normEquity step eqty) bs)


relativeBroom :: Broom (History Equity) -> Broom (History Yield)
relativeBroom (Broom bs) =
  let g (_, Equity old) (b, Equity new) = (b, Yield (new / old)) 
      f (History vs) =
        let start = fmap (const (Yield 1)) (Vec.head vs)
        in History $ Vec.cons start (Vec.zipWith g vs (stail "relativeBroom" vs))
  in Broom (map f bs)


type Percent = Double

data TWR
data Risk

newtype CDF a = CDF {
  unCDF :: Vector (Percent, Double)
  } deriving (Show)

-- twr = terminal wealth relative
terminalWealthRelative :: Equity -> Broom (History Equity) -> CDF TWR -- Vector (Percent, Double)
terminalWealthRelative (Equity e) (Broom hs) =
  let twrs = List.sort (map ((/e) . unEquity . snd . Vec.last . unHistory) hs)
      len = fromIntegral (length twrs)
      g i w = (i/len, w)
  in CDF (Vec.fromList (zipWith g [0..] twrs))

risk :: Broom (History Equity) -> CDF Risk -- Vector (Percent, Double)
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

      
      qs = map (f . Vec.map (unEquity . snd) . unHistory) hs

      h i w = (i/len, w)

  in CDF (Vec.fromList (zipWith h [0..] (List.sort qs)))

data Distribution = Distribution {
  twrCDF :: [(Fraction, CDF TWR)]
  , riskCDF :: [(Fraction, CDF Risk)]
  } deriving (Show)

-- mcout2dist ::
  
