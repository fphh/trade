

module Trade.Test.BlackScholes where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Word (Word32)

import Trade.Type.Equity (Equity(..))


import Trade.Test.Time (year)

import Trade.Test.Wiener (wienerDt, wienerDtDeterministic, Dt(..))


import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist

import Debug.Trace

newtype Mu = Mu { _mu :: Double }
newtype Sigma = Sigma { _sigma :: Double }
newtype Start = Start { _start :: Double }


{-
blackScholes :: Vector UTCTime -> Double -> Double -> Double -> IO (Vector (UTCTime, Double))
blackScholes interval dt start mu sigma = do
  ws <- wienerDt (Vec.length interval) dt
  return (Vec.zip interval ws)
-}

blackScholesDet :: Word32 -> Vector UTCTime -> Equity -> Mu -> Sigma -> IO (Vector (UTCTime, Double))
blackScholesDet seed interval (Equity eqty) (Mu mu) (Sigma sigma) = do
  gen <- MWC.initialize (Vec.singleton seed)
  let dt = 1 / fromIntegral (Vec.length interval)
      g _ = fmap (\z -> mu * dt + sigma * sqrt dt * z) (Dist.normal 0 1 gen)
  rs <- Vec.generateM (Vec.length interval) g
  let f acc r = acc * exp r
      ss = Vec.scanl f eqty rs
  return (Vec.zip interval ss)
