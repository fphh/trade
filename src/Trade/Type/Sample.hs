
module Trade.Type.Sample where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec

import Trade.Type.Signal (Signal(..), Timeseries)


data Sample x = Sample {
  splitIndex :: Int
  , timeseries :: [Timeseries x]
  }

bsplit :: Int -> Double -> Timeseries x -> Sample x
bsplit n q (Signal vs) =
  let k = floor (fromIntegral n * q)
      l = n - k
      go xs | Vec.length xs == 0 = []
      go xs = Signal (Vec.take n xs) : go (Vec.drop l xs)
  in case (Vec.length vs - n) `mod` l of
       0 -> Sample k (go vs)
       _ -> error "bsplit: could not use whole sample"



startTime :: Timeseries x -> UTCTime
startTime (Signal vs) = fst (Vec.head vs)

startPrice :: Timeseries x -> x
startPrice (Signal vs) = snd (Vec.head vs)

splitTime :: Int -> Timeseries x -> UTCTime
splitTime n (Signal vs) = fst (vs Vec.! n)

splitPrice :: Int -> Timeseries x -> x
splitPrice n (Signal vs) = snd (vs Vec.! n)


{-
splitIndex :: Double -> Vector x -> Int
splitIndex q _ | q < 0 || q > 1 = error "Trade.Type.Signal.Price.split: q should be between 0 and 1"
splitIndex q vs = floor (fromIntegral (Vec.length vs) * q)
-}


{-
split :: Double -> Timeseries x -> Sample x
split q _ | q < 0 || q > 1 = error "Trade.Type.Signal.Price.split: q should be between 0 and 1"
split q sig@(Signal vs) =
  let n = floor (q * fromIntegral (Vec.length vs))
  in Sample sig n

bsplit :: Int -> Double -> Timeseries x -> [Sample x]
bsplit n q (Signal vs) =
  let k = floor (fromIntegral n * q)
      l = n - k
      go xs | n > Vec.length xs = []
      go xs =
        let as = Vec.take n xs
        in Sample (Signal as) k : go (Vec.drop l xs)
  in go vs


startTime :: Sample x -> UTCTime
startTime (Sample (Signal vs) i) = fst (Vec.head vs)

startPrice :: Sample x -> x
startPrice (Sample (Signal vs) i) = snd (Vec.head vs)

splitTime :: Sample x -> UTCTime
splitTime (Sample (Signal vs) i) = fst (vs Vec.! i)

splitPrice :: Sample x -> x
splitPrice (Sample (Signal vs) i) = snd (vs Vec.! i)

cutInSample :: Sample x -> Timeseries x
cutInSample (Sample (Signal vs) i) = Signal (Vec.take i vs)

-}

