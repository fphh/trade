{-# LANGUAGE FlexibleContexts #-}


module Trade.Test.Time where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorianValid)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (catMaybes)




months :: Integer -> [Vector UTCTime]
months y =
  let ms = [1 .. 12]
      ds = [1 .. 31]
      g d = UTCTime d 0
      f m = Vec.fromList (map g (catMaybes (map (fromGregorianValid y m) ds)))
  in map f ms

months2017 :: [Vector UTCTime]
months2017@([jan2017, feb2017, mar2017, apr2017, may2017, jun2017, jul2017, aug2017, sept2017, okt2017, nov2017, dez2017]) = months 2017

year :: Integer -> Vector UTCTime
year y = Vec.concat (months y)

years :: Vector UTCTime
years = Vec.concat (map year [2010 .. 2017])

yearsN :: Integer -> Vector UTCTime
yearsN n = Vec.concat (map year [2017-n+1 .. 2017])
