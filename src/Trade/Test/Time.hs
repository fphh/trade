{-# LANGUAGE FlexibleContexts #-}


module Trade.Test.Time where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Text.Printf (printf)

import Trade.Timeseries.Time (parseDate)


import Debug.Trace



months :: [Vector UTCTime]
months@([jan, feb, mar, apr, may, jun, jul, aug, sept, okt, nov, dez]) =
  let f m d =
        let g d = parseDate (printf "2017-%02d-%02d" m d)
        in case Vec.sequence (Vec.map g (Vec.generate d (+1))) of
             Nothing -> error "Trade.Test.Time.months"
             Just xs -> xs
  in zipWith f [1::Int ..12] [31 :: Int, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

year :: Vector UTCTime
year = Vec.concat months

days :: Int -> Vector UTCTime
days n | n <= 365 = Vec.take n year
days _ = error "Trade.Test.Time.days: not enough days in year"


--jan = Vec.fromList $ map ((\(Just t) -> t) . parseDate . ("2019-01-"++) . printf "%02d") [1 :: Integer .. 31]
--feb = Vec.fromList $ map ((\(Just t) -> t) . parseDate . ("2019-02-"++) . printf "%02d") [1 :: Integer .. 28]
--mar = Vec.fromList $ map ((\(Just t) -> t) . parseDate . ("2019-03-"++) . printf "%02d") [1 :: Integer .. 31]
--apr = Vec.fromList $ map ((\(Just t) -> t) . parseDate . ("2019-04-"++) . printf "%02d") [1 :: Integer .. 30]
--may = Vec.fromList $ map ((\(Just t) -> t) . parseDate . ("2019-05-"++) . printf "%02d") [1 :: Integer .. 31]
