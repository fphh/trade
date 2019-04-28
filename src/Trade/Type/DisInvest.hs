


module Trade.Type.DisInvest where

import Data.Map (Map)

data DisInvest =
  Invest
  | Disinvest
  deriving (Eq, Show)

newtype InvestSignal t = InvestSignal {
  unInvestSignal :: Map t DisInvest
  } deriving (Show)
