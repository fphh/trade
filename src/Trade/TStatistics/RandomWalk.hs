

module Trade.TStatistics.RandomWalk where

import Control.Monad


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import System.Random.MWC
import System.Random.MWC.CondensedTable


newtype RandomWalk t a = RandomWalk { unRandomWalk :: Vector a } deriving (Show)

data O
data Z

uniformSampleFrom :: Int -> Vector a -> IO (Vector a)
uniformSampleFrom n vs = do
  let p = 1 / fromIntegral (Vec.length vs)
      us = Vec.map (\v -> (v,p)) vs
      table0 = tableFromProbabilities us
  g <- createSystemRandom
  Vec.sequence (Vec.replicate n (genFromTable table0 g))

randomWalk :: (Num a) => Int -> Vector a -> IO (RandomWalk O a)
randomWalk n vs = uniformSampleFrom n vs >>= return . RandomWalk . Vec.prescanl (+) 0

newtype RWLength = RWLength Int

randomWalks :: (Num a) => Int -> RWLength -> Vector a -> IO [RandomWalk O a]
randomWalks j (RWLength n) = replicateM j . randomWalk n

---------------------------------------------------------------

data RandomWalkGen a = RandomWalkGen {
  numOfWalks :: !Int
  , table :: IO a
  }

prepareRandomWalk :: Int -> Vector a -> IO (RandomWalkGen a)
prepareRandomWalk n vs = do
  let p = 1 / fromIntegral (Vec.length vs)
      us = Vec.map (\v -> (v,p)) vs
      table0 = tableFromProbabilities us
  g <- createSystemRandom
  return $ RandomWalkGen n (genFromTable table0 g)

uniformSampleFrom2 :: RandomWalkGen a -> IO (Vector a)
uniformSampleFrom2 (RandomWalkGen n g) = Vec.sequence (Vec.replicate n g)

randomWalk2 :: (Num a) => RandomWalkGen a -> IO (RandomWalk O a)
randomWalk2 gen = uniformSampleFrom2 gen >>= return . RandomWalk . Vec.prescanl' (+) 0

randomWalks2 :: (Num a) => Int -> RWLength -> Vector a -> IO [RandomWalk O a]
randomWalks2 j (RWLength n) vs = prepareRandomWalk n vs >>= replicateM j . randomWalk2


-----------------------------------------------------------------------------

newtype Zero a = Zero { unZero :: a } deriving (Show)
 
shiftRandomWalk :: (Num a) => Zero a -> RandomWalk O a -> RandomWalk Z a
shiftRandomWalk (Zero z) (RandomWalk rs) = RandomWalk (Vec.map (+z) rs)


absDrawdown :: (Fractional a, Ord a) => RandomWalk Z a -> a
absDrawdown (RandomWalk rs) = (Vec.head rs - Vec.minimum rs) / Vec.head rs

