{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Type.Bars where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Graphics.Rendering.Chart.Easy as E

class Add t where
  data DeltaTy t :: *
  add :: DeltaTy t -> t -> t
  diff :: t -> t -> DeltaTy t

-- somehow fishy, because Num is not really implemented
instance Add UTCTime where
  data DeltaTy UTCTime = NDT {
    unNDT :: NominalDiffTime
    } deriving (Eq, Ord, Num) -- , Real)

  add (NDT dt) t = addUTCTime dt t
  diff x y = NDT (diffUTCTime x y)

instance Real (DeltaTy UTCTime) where
  toRational (NDT t) = toRational t

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
