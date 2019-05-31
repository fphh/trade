{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Strategy.Type where


import Data.Time.Clock (UTCTime)

import Data.Map (Map)
import Data.Vector (Vector)

import Trade.Type.Signal (Timeseries)
import Trade.Type.Strategy.Index (Index)

newtype Focus = Focus {
  unFocus :: Int
  } deriving (Eq, Ord, Show)

newtype Offset = Offset {
  unOffset :: Int
  } deriving (Eq, Ord, Num, Show)


newtype Window = Window {
  unWindow :: Int
  } deriving (Eq, Ord, Num)

instance Show Window where
  show (Window x) = show x

newtype K = K {
  unK :: Double
  } deriving (Eq, Ord)

instance Show K where
  show (K k) = show k

data Modified sym =
  Now sym
  | MAvg Window sym
  | StdDev Window K sym
  deriving (Show, Eq, Ord)

newtype Signals sym x = Signals {
  signals :: Map (Modified sym) (Timeseries x)
  } deriving (Show)

data AlignedSignals sym x = AlignedSignals {
  alignedTimes :: Vector UTCTime
  , modifiedSignals :: Map (Modified sym) (Index -> Focus, Vector x)
  }
  
data IndexedSignals sym x = IndexedSignals {
  index :: Index
  , alignedSignals :: AlignedSignals sym x
  }

newtype Statistics sym = Statistics {
  stats :: Map (Modified sym) Double
  } deriving (Show)


{-
data DisInvest = Invest | Disinvest

newtype InvestSignal t = InvestSignal {
  unInvestSignal :: Map t DisInvest
  }
-}
