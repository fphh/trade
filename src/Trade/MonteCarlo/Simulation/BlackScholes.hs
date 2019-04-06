{-# LANGUAGE TypeFamilies #-}


module Trade.MonteCarlo.Simulation.BlackScholes where

import Control.Monad (replicateM)

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Word (Word32)

import Control.Monad.Primitive (PrimState)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as Dist

-- import Trade.Type.Bars (DeltaTy(Bars), BarNo(..))
import Trade.Type.Bars (Bars(..), BarNo(..))

import Trade.Type.Broom (Broom(..))
-- import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))


newtype Mu = Mu {
  unMu :: Double
  } deriving (Show)

newtype Sigma = Sigma {
  unSigma :: Double
  } deriving (Show)

{-
newtype Start = Start {
  unStart :: Double
  } deriving (Show)
-}


blackScholes ::
  MWC.Gen (PrimState IO) -> Vector t -> Price -> Mu -> Sigma -> IO (Vector (t, Price))
blackScholes gen interval prc (Mu mu) (Sigma sigma) = do
  let dt = 1 / fromIntegral (Vec.length interval)
      g _ = fmap (\z -> mu * dt + sigma * sqrt dt * z) (Dist.normal 0 1 gen)
  rs <- Vec.generateM (Vec.length interval) g
  let f (Price acc) r = Price (acc * exp r)
      ss = Vec.scanl f prc rs
  return (Vec.zip interval ss)


blackScholesDet :: Word32 -> Vector t -> Price -> Mu -> Sigma -> IO (Signal t Price)
blackScholesDet seed interval prc mu sigma =
  MWC.initialize (Vec.singleton seed)
  >>= \gen -> fmap Signal (blackScholes gen interval prc mu sigma)

  {-
priceSignalBroom :: Bars -> Int -> Price -> Mu -> Sigma -> IO (Broom (Signal BarNo Price))
priceSignalBroom (Bars b) n prc mu sigma = do
  let vs = Vec.generate b BarNo
  gen <- MWC.createSystemRandom
  broom <- replicateM n (blackScholes gen vs prc mu sigma)
  return (Broom (map (Signal . Vec.map (fmap Price)) broom))
-}
