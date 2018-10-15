{-# LANGUAGE FlexibleContexts #-}


module Trade.Test.Time where

import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (isLeapYear)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Text.Printf (printf)

import Trade.Timeseries.Time (parseDate)


import Debug.Trace



months :: Integer -> [Vector UTCTime]
months y =
  let f m d =
        let g d = parseDate (printf "%d-%02d-%02d" y m d)
        in case Vec.sequence (Vec.map g (Vec.generate d (+1))) of
             Nothing -> error "Trade.Test.Time.months"
             Just xs -> xs

      febN = case isLeapYear y of
               True -> 29
               False -> 28
               
  in zipWith f [1::Int ..12] [31 :: Int, febN, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


months2017 :: [Vector UTCTime]
months2017@([jan2017, feb2017, mar2017, apr2017, may2017, jun2017, jul2017, aug2017, sept2017, okt2017, nov2017, dez2017]) = months 2017

year :: Integer -> Vector UTCTime
year y = Vec.concat (months y)

years :: Vector UTCTime
years = Vec.concat (map year [2010 .. 2017])

yearsN :: Integer -> Vector UTCTime
yearsN n = Vec.concat (map year [2017-n+1 .. 2017])
