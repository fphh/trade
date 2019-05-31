
module Trade.Strategy.Library.BuyAndHold where

import Control.Monad (void)

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.Signal (Timeseries)
import Trade.Type.DisInvest (DisInvest(..), InvestSignal)


import Trade.Type.Add (Add)
import Trade.Type.Scale (Scale)

import Trade.Strategy.Algorithm (time, now, start, end)
import Trade.Strategy.Process (process)
import Trade.Strategy.Type (Signals, AlignedSignals)

import Trade.Strategy.Condition (symbol, conditions, (.=), Condition((:=:)), Implication((:->)))

import Trade.Statistics.Algorithm (Statistics)


buyAndHold ::
  (Ord sym, Statistics x, Scale x, Add x) =>
  [(sym, Timeseries x)] -> State (Signals sym x) (AlignedSignals sym x, Map sym InvestSignal)
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

