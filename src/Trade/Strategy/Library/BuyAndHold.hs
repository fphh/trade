
module Trade.Strategy.Library.BuyAndHold where

import Control.Monad (void)

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.DisInvest (DisInvest(..), InvestSignal)
import Trade.Type.Signal (Signal)
import qualified Trade.Type.Signal as Signal

import Trade.Strategy.Type (Signals)

import Trade.Strategy.Algorithm (time, now)
import Trade.Strategy.Condition (symbol, constant, conditions, (.=), Condition((:=:)), Implication((:->)))
import Trade.Strategy.Process (process)



buyAndHold ::
  (Ord t, Ord sym, Fractional x) =>
  (sym, Signal t x) -> State (Signals sym t x) (Map sym (InvestSignal t))
buyAndHold vs@(_, xs) = do

  let t0 = constant (fst (Signal.head xs))
      tn = constant (fst (Signal.last xs))
      
  void (now vs)
  
  process $ do
    
    p <- time 0
    
    conditions $
      
      symbol vs :=: [ p .= t0 :-> Invest
                    , p .= tn :-> Disinvest ]
