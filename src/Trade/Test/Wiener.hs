

module Trade.Test.Wiener where

import Control.Monad (replicateM)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Word (Word32)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist

newtype Dt = Dt { _dt :: Double }

wienerDt :: Int -> Dt -> IO (Vector Double)
wienerDt n (Dt dt) =
  MWC.withSystemRandom . MWC.asGenST
  $ \gen -> fmap Vec.fromList (replicateM n (Dist.normal 0 dt gen))


wienerDtDeterministic :: Word32 -> Int -> Dt -> IO (Vector Double)
wienerDtDeterministic seed n (Dt dt) = do
  gen <- MWC.initialize (Vec.singleton seed)
  fmap Vec.fromList (replicateM n (Dist.normal 0 dt gen))



{-
wiener :: Int -> IO (Vector Double)
wiener n = do
  vs <- MWC.withSystemRandom . MWC.asGenST $
    \gen -> fmap Vec.fromList (replicateM n (MWC.uniformR (-1.0, 1.0) gen))
  return (Vec.scanl (+) 0 vs)


wienerDeterministic :: Word32 -> Int -> IO (Vector Double)
wienerDeterministic seed n = do
  gen <- MWC.initialize (Vec.singleton seed)
  vs <- replicateM n (MWC.uniformR (-1.0, 1.0) gen)
  return (Vec.scanl (+) 0 (Vec.fromList vs))

-}
