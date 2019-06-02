{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.Signal where

import Prelude hiding (scanl, map, zipWith, head, last, tail)

import Data.Time.Clock (UTCTime, NominalDiffTime)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import qualified Data.Foldable as Fold

import Data.Function (on)

import Trade.Report.Pretty (Pretty)

import Trade.Help.SafeTail (stail, shead, slast)



newtype Signal t y = Signal {
  unSignal :: Vector (t, y)
  } deriving (Show, Read, Eq, Semigroup, Monoid)

instance Functor (Signal t) where
  fmap f (Signal ps) = Signal (Vec.map (fmap f) ps)

type Timeseries = Signal UTCTime
type DeltaTimeseries = Signal NominalDiffTime

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

head :: Signal t y -> (t, y)
head (Signal ps) = shead "Signal.head" ps

tail :: Signal t y -> Signal t y
tail (Signal ps) = Signal (stail "Signal.head" ps)

last :: Signal t y -> (t, y)
last (Signal ps) = slast "Signal.tail" ps

mapFirst :: (y -> y) -> Signal t y -> Signal t y
mapFirst f (Signal ps)
  | Vec.length ps == 0 = error "Signal.mapFirst: empty vector"
  | otherwise = Signal (ps Vec.// [(1, fmap f (Vec.head ps))])

mapLast :: (y -> y) -> Signal t y -> Signal t y
mapLast f (Signal ps)
  | Vec.length ps == 0 = error "Signal.mapLast: empty vector"
  | otherwise = Signal (ps Vec.// [(Vec.length ps - 1, fmap f (Vec.last ps))])

length :: Signal x y -> Int
length (Signal xs) = Vec.length xs

empty :: Signal t x
empty = Signal (Vec.empty)

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


drop :: Int -> Signal t x -> Signal t x
drop n (Signal xs) = Signal (Vec.drop n xs)


adjust :: (Ord t) => x -> Vector t -> Signal t x -> Signal t x
adjust e ts (Signal xs)
  | Vec.length xs == 0 = error "Signal.adjust: empty vector"
  | otherwise =
    let (t0, _) = Vec.head xs
        (tn, yn) = Vec.last xs
        (as, bs) = Vec.span (< t0) ts
        cs = Vec.dropWhile (<= tn) bs
    in Signal
       $ Vec.concat
       [ Vec.map (\t -> (t, e)) as
       , xs
       , Vec.map (\t -> (t, yn)) cs ]
       

downsample :: (Ord t) => Set t -> Signal t x -> Signal t x
downsample ts (Signal xs) =
  let p (t, _) = Set.member t ts
      ys = Vec.filter p xs
  in Signal ys 


upsample :: (Ord t) => Set t -> Signal t x -> Signal t x
upsample ts (Signal xs) = error "Signal.upsample not defined"
{-
  let vs = Vec.fromList (Set.toList ts)
      f (t0, y0) (t1, y1) = (t0, t1, \a -> y0 + (y1-y0)/(fromIntegral (t1-t0)) * (a - fromIntegral t0))
      is = Vec.zipWith f xs (Vec.tail xs)
  in error "Signal.upsample not defined"
-}
