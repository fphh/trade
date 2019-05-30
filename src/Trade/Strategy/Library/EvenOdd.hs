
module Trade.Strategy.Library.EvenOdd where

import Control.Monad (void)

import Control.Monad.State (State)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Signal (Signal)
import Trade.Type.DisInvest (DisInvest(..), InvestSignal)


import Trade.Type.Add (Add)
import Trade.Type.Scale (Scale)

import Trade.Strategy.Algorithm (time, now, start, end, index)
import Trade.Strategy.Process (process)
import Trade.Strategy.Type (Signals, AlignedSignals)

import Trade.Strategy.Condition (symbol, conditions, (.=), Condition((:=:)), Implication((:->)))

import Trade.Statistics.Algorithm (Statistics)


evenOdd ::
  (Ord t, Ord sym, Statistics x, Scale x, Add x) =>
  Map sym (Signal t x) -> State (Signals sym t x) (AlignedSignals sym t x, Map sym InvestSignal)
evenOdd ms | Map.null ms = error "evenOdd"
evenOdd ms = do

  let vs:_ = Map.toList ms
  
  void (now vs)
  
  process $ do
       
    idx <- index
      
    conditions $

      symbol vs :=: [ Just (even idx) :-> Invest
                    , Just (odd idx) :-> Disinvest ]

