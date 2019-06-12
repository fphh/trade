
module Trade.Type.Sample where

import qualified Data.Vector as Vec

import Trade.Type.Signal (Signal(..), Timeseries)


data Sample x = Sample {
  inSample :: Timeseries x
  , outOfSample :: Timeseries x
  } deriving (Show, Read)

split :: Double -> Timeseries x -> Sample x
split q _ | q < 0 || q > 1 = error "Trade.Type.Signal.Price.split: q should be between 0 and 1"
split q (Signal vs) =
  let n = floor (q * fromIntegral (Vec.length vs))
      (i, o) = Vec.splitAt n vs
  in Sample (Signal i) (Signal o)


emptySample :: Sample x
emptySample = Sample mempty mempty
