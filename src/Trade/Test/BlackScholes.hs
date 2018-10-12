

module Trade.Test.BlackScholes where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Word (Word32)


import Trade.Test.Time (year)

import Trade.Test.Wiener (wiener, wienerDeterministic)

blackScholes :: Vector UTCTime -> Double -> Double -> Double -> IO (Vector (UTCTime, Double))
blackScholes interval start mu sigma = do
  ws <- wiener (Vec.length interval)
  return (Vec.zip interval ws)

blackScholesDeterministic ::
  Word32 -> Vector UTCTime -> Double -> Double -> Double -> IO (Vector (UTCTime, Double))
blackScholesDeterministic seed interval start mu sigma = do
  ws <- wienerDeterministic seed (Vec.length interval)
  return (Vec.zip interval (Vec.map (+start) ws))
