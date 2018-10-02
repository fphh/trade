

module Trade.Type.Signal where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Report.NumberedList
import Trade.Report.Pretty



newtype Signal t x = Signal {
  unSignal :: Vector (t, x)
  } deriving (Show, Read)

instance (Pretty x, Pretty t) => ToNumberedList (Signal t x) where
  toNumberedList (Signal pps) = toNumberedList pps

data OffsettedSignal t x = OffsettedSignal {
  offset :: t
  , signal :: Signal t x
  } deriving (Show, Read)


data Sample t x = Sample {
  inSample :: Signal t x
  , outOfSample :: Signal t x
  } deriving (Show, Read)

split :: Double -> Signal t x -> Sample t x
split q _ | q < 0 || q > 1 = error "Trade.Type.Signal.Price.split: q should be between 0 and 1"
split q (Signal vs) =
  let n = floor (q * fromIntegral (Vec.length vs))
      (i, o) = Vec.splitAt n vs
  in Sample (Signal i) (Signal o)

noSignal :: Signal t x
noSignal = Signal (Vec.empty)

noSample :: Sample t x
noSample = Sample noSignal noSignal
