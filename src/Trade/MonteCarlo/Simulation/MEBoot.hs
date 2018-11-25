

module Trade.MonteCarlo.Simulation.MEBoot where

import Control.Monad (replicateM)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Control.Monad.Primitive (PrimState)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist

import Trade.Type.Signal (Signal(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Broom (Broom(..))

import qualified Data.List as List

{-
xs :: [Double]
xs = [4, 12, 36, 20, 8]

ps :: [Double]
ps = [0.12, 0.83, 0.53, 0.59, 0.11]
-}


intermediatePts :: [Double] -> [Double]
intermediatePts xs = zipWith (\x y -> (x+y)/2) (tail xs) xs
  
trimmedCnt :: Double -> [a] -> Int
trimmedCnt p xs =
  let len = fromIntegral (length xs)
  in floor (p*len)

trimmedMean :: Double -> [Double] -> Double
trimmedMean p xs =
  let tc = trimmedCnt p xs
      ys = iterate init (drop tc xs) !! tc
  in sum ys / fromIntegral (length ys)

approx :: (Double, Double) -> (Double, Double) -> Double -> Double
approx (x0, x1) (y0, y1) x =
  let m = (y1-y0) / (x1-x0)
  in y1 + m * (x - x1)

keepOrder :: (Ord a, Show a) => ([a] -> [b]) -> [a] -> [b]
keepOrder f =
  map snd
  . List.sortOn fst
  . uncurry zip
  . fmap f
  . unzip
  . List.sortOn snd
  . zip ([0..] :: [Integer])

cdfs ::
  [(Double, (Double, Double))]
  -> [((Double, Double),Double, (Double, Double), Double -> Double)]
cdfs [] = error "cdf: no intervals"
cdfs xs =
  let len = fromIntegral (length xs)
      f i (m, (l, r)) = ((i/len, (i+1)/len), m, (l, r), approx (i/len, (i+1)/len) (l, r))
  in zipWith f [0, 1 ..] xs

           
means :: [Double] -> [Double]
means =
  let go _ [a, b] = [0.25*a + 0.75*b]
      go 0 (a:b:ks) = (0.75*a + 0.25*b) : go 1 (a:b:ks)
      go n (a:b:c:ks) = (0.25*a + 0.5*b + 0.25*c) : go (n+1) (b:c:ks)
      go _ _ = error "means: never here"
  in go (0 :: Integer)


meboot' :: [Double] -> [Double] -> [Double]
meboot' ps xs =
  let ipts = intermediatePts xs
      trm = trimmedMean 0.1 ipts
      xmin = head xs - trm
      xmax = last xs + trm
      zs = xmin : ipts ++ [xmax]
      ms = means xs
      is = zip ms (zip zs (tail zs))
      cs = cdfs is
      
      g p =
        case List.find (\((pl, pr), _, _, _) -> pl <= p && p < pr) cs of
          Just (_, m, (l, r), c) -> c p + m - 0.5*(l+r)
          Nothing -> error ("not a probability: " ++ show p)

  in map g (List.sort ps)


mebootList :: [Double] -> [Double] -> [Double]
mebootList ps = keepOrder (meboot' ps)

mebootWithGen ::
  MWC.Gen (PrimState IO) -> Vector (t, Double) -> IO (Vector (t, Double))
mebootWithGen gen vs = do
  rs <- Vec.generateM (Vec.length vs) (const (MWC.uniform gen))
  let (ts, ys) = Vec.unzip vs
      ys' = Vec.toList ys
      rs' = Vec.toList rs
      newYs = Vec.fromList (mebootList rs' ys')
  return (Vec.zip ts newYs)

meboot :: Signal t Double -> IO (Signal t Double)
meboot (Signal xs) = do
  gen <- MWC.createSystemRandom
  ys <- mebootWithGen gen xs
  return (Signal ys)
  
mebootBroom :: Int -> Signal t Price -> IO (Broom (Signal t Price))
mebootBroom n (Signal xs) = do
  gen <- MWC.createSystemRandom
  broom <- replicateM n (mebootWithGen gen (Vec.map (fmap unPrice) xs))
  return (Broom (map (Signal . Vec.map (fmap Price)) broom))
