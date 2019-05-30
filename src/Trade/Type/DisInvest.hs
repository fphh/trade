


module Trade.Type.DisInvest where

import Data.Map (Map)

import Trade.Type.Strategy.Index (Index)

data DisInvest =
  Invest
  | Disinvest
  deriving (Eq, Show)

newtype InvestSignal = InvestSignal {
  unInvestSignal :: Map Index DisInvest
  } deriving (Show)
