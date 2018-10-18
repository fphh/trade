{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Type.Signal where

import Prelude hiding (scanl, map, zipWith, head, last, tail)

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Report.NumberedList (ToNumberedList, toNumberedList)
import Trade.Report.Pretty (Pretty)
import Trade.Report.Line (Line(..), L(..))

import Trade.Help.SafeTail (stail, shead, slast)


newtype Signal t y = Signal {
  unSignal :: Vector (t, y)
  } deriving (Show, Read, Eq)

instance Functor (Signal t) where
  fmap f (Signal ps) = Signal (Vec.map (fmap f) ps)

instance Line (Signal x y) where
  type TyX (Signal x y) = x
  type TyY (Signal x y) = y
  line str (Signal ps) = L str (Vec.toList ps)
  

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


map :: ((p, q) -> (r, s)) -> Signal p q -> Signal r s
map f (Signal xs) = Signal (Vec.map f xs)

scanl :: (b -> a -> b) -> b -> Signal t a -> Signal t b
scanl f x (Signal xs) =
  let start = fmap (const x) (Vec.head xs)
      g (_, x) (t, y) = (t, f x y)
  in Signal (Vec.scanl g start (Vec.tail xs))

zipWith ::(a -> a -> b) -> b -> Signal t a -> Signal t b
zipWith f x (Signal xs) =
  let start = fmap (const x) (Vec.head xs)
      g (_, x) (t, y) = (t, f x y)
  in Signal (Vec.cons start (Vec.zipWith g xs (stail "Signal.zipWith" xs)))

test_scanl_zipWith :: (Eq t, Eq a, Fractional a) => Signal t a -> Bool
test_scanl_zipWith ps =
  let qs = zipWith (flip (/)) 0 ps
      (_, start) = head ps
      rs = scanl (*) start qs
  in ps == rs

head :: Signal t y -> (t, y)
head (Signal ps) = shead "Signal.head" ps

tail :: Signal t y -> Signal t y
tail (Signal ps) = Signal (stail "Signal.head" ps)

last :: Signal t y -> (t, y)
last (Signal ps) = slast "Signal.tail" ps

length :: Signal x y -> Int
length (Signal xs) = Vec.length xs

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
