

module Trade.Algorithm.Bollinger where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Statistics.Sample (mean, stdDev)

-- import Trade.Render.Svg.Plot

import Debug.Trace

-- number stddev
newtype BolK = BolK Int deriving (Show)

-- rolling window
newtype BolWin = BolWin Int deriving (Show)

meanStDev :: BolWin -> Vector Double -> (Vector (Double, Double), Vector (Double, Double))
meanStDev (BolWin l) v
  | l > Vec.length v = error $ "meanStDev: input vector to short: " ++ show l ++ "/" ++ show (Vec.length v)
meanStDev (BolWin l) v =
  let len = Vec.length v
      f a =
        let u = Vec.slice a l v
        in (mean u, stdDev u)
      idx = Vec.generate (len-l) (fromIntegral . (l+))
      (ms, ss) = Vec.unzip $ Vec.generate (len-l) f
  in (Vec.zip idx ms, Vec.zip idx ss)
  
data Bollinger = Bollinger {
  n :: BolWin
  , k :: BolK
  , bMean :: Vector (Double, Double)
  , bStDev :: Vector (Double, Double)
  } deriving (Show)


bollinger :: BolK -> BolWin -> Vector Double -> Bollinger
bollinger k n = uncurry (Bollinger n k) . meanStDev n


{-
bollinger2Lines :: String -> BolK -> BolWin -> Vector Double -> [PlotItem Vector Double]
bollinger2Lines str k@(BolK j) n v =
  let Bollinger _ _ ms ss = bollinger k n v
      k' = fromIntegral j
      f j (i, m) (_, s) = (i, m + j*s)
  in [ Line (str ++ " &mu;") ms
     , Line (str ++ " " ++ show j ++ "&sigma;") (Vec.zipWith (f k') ms ss)
     , Line (str ++ " " ++ show j ++ "&sigma;") (Vec.zipWith (f (-k')) ms ss) ]
-}
