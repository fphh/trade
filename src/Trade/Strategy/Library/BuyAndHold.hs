
module Trade.Strategy.Library.BuyAndHold where

import Control.Monad (void)

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.Signal (Signal)
import Trade.Type.DisInvest (DisInvest(..), InvestSignal)


import Trade.Type.Add (Add)
import Trade.Type.Scale (Scale)

import Trade.Strategy.Algorithm (time, now, start, end)
import Trade.Strategy.Process (process)
import Trade.Strategy.Type (Signals, AlignedSignals)

import Trade.Strategy.Condition (symbol, conditions, (.=), Condition((:=:)), Implication((:->)))

import Trade.Statistics.Algorithm (Statistics)


buyAndHold ::
  (Ord t, Ord sym, Statistics x, Scale x, Add x) =>
  [(sym, Signal t x)] -> State (Signals sym t x) (AlignedSignals sym t x, Map sym (InvestSignal t))
buyAndHold [] = error "buyAndHold"
buyAndHold (vs:_) = do

  void (now vs)
  
  process $ do
       
    t0 <- start
    tn <- end
 
    p <- time 0
      
    conditions $

      symbol vs :=: [ p .= t0 :-> Invest
                    , p .= tn :-> Disinvest ]

