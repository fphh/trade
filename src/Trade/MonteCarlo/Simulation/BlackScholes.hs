{-# LANGUAGE TypeFamilies #-}


module Trade.MonteCarlo.Simulation.BlackScholes where

import Control.Monad (replicateM)

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Word (Word32)

import Control.Monad.Primitive (PrimState)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist


import Trade.Type.Bars (Bars(..), BarNo(..))
import Trade.Type.Broom (Broom(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.History (History(..))
import Trade.Type.Signal.Price (PriceSignal)
import qualified Trade.Type.OHLC as O

import Trade.Timeseries.OHLC (OHLC(..))

import Debug.Trace

newtype Mu = Mu { _mu :: Double }
newtype Sigma = Sigma { _sigma :: Double }
newtype Start = Start { _start :: Double }



blackScholes ::
  MWC.Gen (PrimState IO) -> Vector a -> Equity -> Mu -> Sigma -> IO (Vector (a, Double))
blackScholes gen interval (Equity eqty) (Mu mu) (Sigma sigma) = do
  let dt = 1 / fromIntegral (Vec.length interval)
      g _ = fmap (\z -> mu * dt + sigma * sqrt dt * z) (Dist.normal 0 1 gen)
  rs <- Vec.generateM (Vec.length interval) g
  let f acc r = acc * exp r
      ss = Vec.scanl f eqty rs
  return (Vec.zip interval ss)

blackScholesDet :: Word32 -> Vector UTCTime -> Equity -> Mu -> Sigma -> IO (Vector (UTCTime, Double))
blackScholesDet seed interval eqty mu sigma =
  MWC.initialize (Vec.singleton seed) >>= \gen -> blackScholes gen interval eqty mu sigma

priceSignalBroom :: Bars -> Int -> Equity -> Mu -> Sigma -> IO (Broom (History Double))
priceSignalBroom (Bars b) n eqty mu sigma = do
  let vs = Vec.generate b BarNo
  gen <- MWC.createSystemRandom
  broom <- replicateM n (blackScholes gen vs eqty mu sigma)
  return (Broom (map History broom))
