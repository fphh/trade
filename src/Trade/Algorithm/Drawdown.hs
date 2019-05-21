{-# LANGUAGE RecordWildCards #-}


module Trade.Algorithm.Drawdown where

import qualified Data.List as List
import qualified Data.Vector as Vec

import qualified Statistics.Sample as Sample

import qualified Trade.Report.Report as Rep

import Trade.Report.HtmlIO (HtmlIO)

{-

dds :: [Double] -> [(Int, Double)]
dds xs =
  let go n a [] = [(n, a)]
      go n 0 (x:xs) = go n x xs
      go n a (0:xs) = go (n+1) a xs
      go n a (x:xs) =
        case a*x > 0 of
          True -> go (n+1) (a+x) xs
          False -> let ys = go 1 x xs in (n, a):ys
  in go 1 0 (zipWith (-) (tail xs) xs)

drawdownSeries :: [Double] -> [(Double, Double)]
drawdownSeries us@(u:_) =
  let go acc [] = [acc]
      go (a, b) ((x, y):us) =
        let vs = go (a+fromIntegral x, b+y) us
        in (a, b) : vs
  in go (0, u) (dds us)
  
drawdowns :: [Double] -> [(Int, Double)]
drawdowns = filter ((<0) . snd) . dds

numberOfDrawdowns :: [Double] -> Int
numberOfDrawdowns = length . drawdowns

maximumDrawdown :: [Double] -> Double
maximumDrawdown = minimum . map snd . dds

wins :: [Double] -> [(Int, Double)]
wins = filter ((>0) . snd) . dds

numberOfWins :: [Double] -> Int
numberOfWins = length . wins

maximumWin :: [Double] -> Double
maximumWin = maximum . map snd . dds

data DrawdownStatistics = DrawdownStatistics {
  numOfWins :: !Int
  , maxWin :: !Double
  , meanWin :: !Double
  , stdDevWin :: !Double
  , numOfDrawdowns :: !Int
  , maxDrawdown :: !Double
  , meanDrawdown :: !Double
  , stdDevDrawdown :: !Double
  } deriving (Show)

drawdownStats :: [Double] -> DrawdownStatistics
drawdownStats xs =
  let ds = map snd (dds xs)
      wins = filter (>0) ds
      vwins = Vec.fromList wins
      losses = filter (<0) ds
      vlosses = Vec.fromList losses
  in DrawdownStatistics {
    numOfWins = length wins
    , maxWin = maximum wins
    , meanWin = Sample.mean vwins
    , stdDevWin = Sample.stdDev vwins
    , numOfDrawdowns = length losses
    , maxDrawdown = minimum losses
    , meanDrawdown = Sample.mean vlosses
    , stdDevDrawdown = Sample.stdDev vlosses
    }

reportDrawdownStats :: [Double] -> HtmlIO
reportDrawdownStats xs =
  let DrawdownStatistics{..} = drawdownStats xs
  in Rep.vtable $
     ["Number of wins", show numOfWins]
     : ["Win max", show maxWin]
     : ["Win mean", show meanWin]
     : ["Win stddev", show stdDevWin]
     : ["----------"]
     : ["Number of drawdowns", show numOfDrawdowns]
     : ["Drawdown max", show maxDrawdown]
     : ["Drawdown mean", show meanDrawdown]
     : ["Drawdown stddev", show stdDevDrawdown]
     : []


-}
