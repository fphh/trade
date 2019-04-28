

module Trade.Strategy.Type where

import Data.Map (Map)
import Data.Vector (Vector)

newtype Index = Index {
  unIndex :: Int
  } deriving (Eq, Ord, Show)

newtype Focus = Focus {
  unFocus :: Int
  } deriving (Eq, Ord, Show)

newtype Offset = Offset {
  unOffset :: Int
  } deriving (Eq, Ord, Show)

data Modified sym =
  Now sym
  | MAvg Int sym
  deriving (Show, Eq, Ord)

newtype Signals sym t x = Signals {
  signals :: Map (Modified sym) (Vector (t, x))
  } deriving (Show)

data AlignedSignals sym t x = AlignedSignals {
  alignedTimes :: Vector t
  , modifiedSignals :: Map (Modified sym) (Index -> Focus, Vector x)
  }

data IndexedSignals sym t x = IndexedSignals {
  index :: Index
  , alignedSignals :: AlignedSignals sym t x
  }

newtype Statistics sym = Statistics {
  stats :: Map (Modified sym) Double
  } deriving (Show)


data DisInvest = Invest | Disinvest

newtype InvestSignal t = InvestSignal {
  unInvestSignal :: Map t DisInvest
  }
