{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trade.Type.Bars where

import Text.Printf (printf)

import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime)

import Data.Time.Calendar (fromGregorian)

import qualified Graphics.Rendering.Chart.Easy as E




class Add t where
  data DeltaTy t :: *
  add :: DeltaTy t -> t -> t
  diff :: t -> t -> DeltaTy t


instance Add UTCTime where
  data DeltaTy UTCTime = NDT {
    unNDT :: NominalDiffTime
    } deriving (Show, Eq, Ord)

  add (NDT dt) t = addUTCTime dt t
  diff x y = NDT (diffUTCTime x y)

-- TODO: Why can't we derive this?
instance Num (DeltaTy UTCTime) where
  NDT x + NDT y = NDT (x+y)
  NDT x * NDT y = NDT (x*y)
  abs (NDT x) = NDT (abs x)
  signum (NDT x) = NDT (signum x)
  fromInteger = NDT . fromInteger
  negate (NDT x) = NDT (negate x)

instance Fractional (DeltaTy UTCTime) where
  fromRational = NDT . fromRational
  recip (NDT x) = NDT (recip x)

instance Real (DeltaTy UTCTime) where
  toRational (NDT x) = toRational x



newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord, E.PlotValue)

newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

{-
-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord, E.PlotValue)

instance Add BarNo where
  -- | Duration.
  data DeltaTy BarNo = Bars {
    unBars :: Int
    } deriving (Show, Eq, Ord)

  add (Bars dt) (BarNo t) = BarNo (dt+t)
  diff (BarNo x) (BarNo y) = Bars (x-y)

-- TODO: Why can't we derive this?
instance Num (DeltaTy BarNo) where
  Bars x + Bars y = Bars (x+y)
  Bars x * Bars y = Bars (x*y)
  abs (Bars x) = Bars (abs x)
  signum (Bars x) = Bars (signum x)
  fromInteger = Bars . fromInteger
  negate (Bars x) = Bars (negate x)
-}
{-
instance Fractional (DeltaTy BarNo) where
  fromRational = Bars . fromRational
  recip (Bars x) = Bars (recip x)
-}


{-
instance Real (DeltaTy UTCTime) where
  toRational (NDT t) = toRational t
-}

{-
instance Num (DeltaTy UTCTime) where
  (NDT a) + (NDT b) = NDT (a+b)
  fromInteger x = NDT (fromInteger x)
-}

{-

class FormatDelta t where
  formatDelta :: DeltaTy t -> String

instance FormatDelta UTCTime where
  formatDelta (NDT dt) = printf "%.2fd" ((fromRational (toRational dt) :: Double) / (24*60*60))

instance FormatDelta BarNo where
  formatDelta (Bars dt) = show dt
-}
