

module Trade.Strategy.Library.NoInvestment where

import Control.Monad (void)

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.DisInvest (InvestSignal)
import Trade.Type.Signal (Signal)

import Trade.Strategy.Type (Signals, AlignedSignals)

import Trade.Strategy.Algorithm (now)
import Trade.Strategy.Condition (symbol, conditions, Condition((:=:)))
import Trade.Strategy.Process (process)


{-
noInvestment ::
  (Ord t, Ord sym, Fractional x, Floating x) =>
  (sym, Signal t x) -> State (Signals sym t x) (AlignedSignals sym t x, Map sym (InvestSignal t))
noInvestment vs = do
  void (now vs)
  process (conditions (symbol vs :=: []))

-}
