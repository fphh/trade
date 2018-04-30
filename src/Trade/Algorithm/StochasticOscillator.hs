

module Trade.Algorithm.StochasticOscillator where


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Algorithm.MovingAverage

import Svg.Plot


-- Probably slow implementation
oscV :: Int -> Vector Double -> Vector (Double, Double)
oscV l v
  | l > Vec.length v = error $ "oscV: input vector to short: " ++ show l ++ "/" ++ show (Vec.length v)
oscV l v = 
  let len = Vec.length v
      f a =
        let u = Vec.slice a l v
            min = Vec.minimum u
            max = Vec.maximum u
            c = Vec.last u
        in (fromIntegral (a+l), 100 * (c - min) / (max - min))
  in Vec.generate (len-l) f

data StochasticOscillator = StochasticOscillator {
  osc :: Vector (Double, Double)
  , threePeriod :: Vector (Double, Double)
  , signal :: Vector (Double, Double)
  } deriving (Show)


oscillator :: Int -> Vector Double -> StochasticOscillator
oscillator l v =
  let o = oscV l v
      os = Vec.map snd o
      tp = mavg 3 os
      l' = fromIntegral l
      f (j, x) = (j+l', x)
  in StochasticOscillator {
    osc = o
    , threePeriod = Vec.map f tp
    , signal = undefined
    }

oscillator2Lines :: String -> Int -> Vector Double -> [PlotItem Vector Double]
oscillator2Lines str l v =
  let StochasticOscillator o tp sig = oscillator l v
  in [ Line (str ++ " osc") o
     , Line (str ++ " tp") tp ]

