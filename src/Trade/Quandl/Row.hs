{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Quandl.Row where

import Data.Time.Clock (UTCTime)

import Data.Map (Map)

import Trade.Quandl.Algorithm.EquityAndShare (PricePerShare)

class RowInterface row where
  dateR :: row -> UTCTime
  openR :: row -> Double
  highR :: row -> Double
  lowR :: row -> Double
  closeR :: row -> PricePerShare
  volumeR :: row -> Double


class DateInterface row where
  dateDI :: row -> UTCTime

instance DateInterface (UTCTime, a) where
    dateDI = fst

instance DateInterface (UTCTime, a, b) where
    dateDI (t, _, _) = t


toDateF2 :: (Functor f, DateInterface t) => (t -> b) -> f t -> f (UTCTime, b)
toDateF2 f = fmap (\x -> (dateDI x, f x))

toDateF :: (Functor f, DateInterface t) => (t -> b) -> Map k (f t) -> Map k (f (UTCTime, b))
toDateF = fmap . toDateF2

