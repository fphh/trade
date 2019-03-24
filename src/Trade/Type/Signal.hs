{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.Signal where

import Prelude hiding (scanl, map, zipWith, head, last, tail)

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Foldable as Fold

import Data.Function (on)

import Trade.Report.NumberedList (ToNumberedList, toNumberedList)
import Trade.Report.Pretty (Pretty)

import Trade.Help.SafeTail (stail, shead, slast)


newtype Signal t y = Signal {
  unSignal :: Vector (t, y)
  } deriving (Show, Read, Eq, Semigroup, Monoid)

instance Functor (Signal t) where
  fmap f (Signal ps) = Signal (Vec.map (fmap f) ps)

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
  in Signal (Vec.cons start (Vec.zipWith g (stail "Signal.zipWith" xs) xs))

{-
TODO: Testing

test_scanl_zipWith :: (Eq t, Eq a, Fractional a) => Signal t a -> Bool
test_scanl_zipWith ps =
  let qs = zipWith (flip (/)) 0 ps
      (_, start) = head ps
      rs = scanl (*) start qs
  in ps == rs
-}

head :: Signal t y -> (t, y)
head (Signal ps) = shead "Signal.head" ps

tail :: Signal t y -> Signal t y
tail (Signal ps) = Signal (stail "Signal.head" ps)

last :: Signal t y -> (t, y)
last (Signal ps) = slast "Signal.tail" ps

mapFirst :: (y -> y) -> Signal t y -> Signal t y
mapFirst f (Signal ps)
  | Vec.length ps == 0 = error "Signal.setLast: empty vector"
  | otherwise =
      let ([(t, y)], ps') = (\(as, bs) -> (Vec.toList as, bs)) (Vec.splitAt 1 ps)
      in Signal ((t, f y) `Vec.cons` ps')

mapLast :: (y -> y) -> Signal t y -> Signal t y
mapLast f (Signal ps)
  | Vec.length ps == 0 = error "Signal.setLast: empty vector"
  | otherwise =
      let (ps', [(t, y)]) = fmap Vec.toList (Vec.splitAt (Vec.length ps-1) ps)
      in Signal (ps' `Vec.snoc` (t, f y))

length :: Signal x y -> Int
length (Signal xs) = Vec.length xs

split :: Double -> Signal t x -> Sample t x
split q _ | q < 0 || q > 1 = error "Trade.Type.Signal.Price.split: q should be between 0 and 1"
split q (Signal vs) =
  let n = floor (q * fromIntegral (Vec.length vs))
      (i, o) = Vec.splitAt n vs
  in Sample (Signal i) (Signal o)

-- empty :: Signal t x
-- empty = Signal (Vec.empty)

emptySample :: Sample t x
emptySample = Sample mempty mempty


singleton :: (t, x) -> Signal t x
singleton = Signal . Vec.singleton


ifoldr' :: (Int -> (t, y) -> a -> a) -> a -> Signal t y -> a
ifoldr' f x (Signal xs) = Vec.ifoldr' f x xs

minimum :: (Ord x, Eq t) => Signal t x -> (t, x)
minimum (Signal xs) = Vec.minimumBy (compare `on` snd) xs

maximum :: (Ord x, Eq t) => Signal t x -> (t, x)
maximum (Signal xs) = Vec.maximumBy (compare `on` snd) xs
