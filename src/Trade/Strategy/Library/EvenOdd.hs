
module Trade.Strategy.Library.EvenOdd where

import Control.Monad (void)

import Control.Monad.Trans.State (State)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Signal (Timeseries)
import Trade.Type.DisInvest (DisInvest(..), InvestSignal)


import Trade.Type.Add (Add)
import Trade.Type.Scale (Scale)

import Trade.Strategy.Algorithm (now, index)
import Trade.Strategy.Process (process)
import Trade.Strategy.Type (Signals, AlignedSignals)

import Trade.Strategy.Condition (symbol, conditions, Condition((:=:)), Implication((:->)))

import Trade.Statistics.Algorithm (Statistics)


evenOdd ::
  (Ord sym, Statistics x, Scale x, Add x) =>
  Map sym (Timeseries x) -> State (Signals sym x) (AlignedSignals sym x, Map sym InvestSignal)
evenOdd ms | Map.null ms = error "evenOdd"
evenOdd ms = do

  let vs:_ = Map.toList ms
  
  void (now vs)
  
  process $ do
       
    idx <- index
      
    conditions $

      symbol vs :=: [ Just (even idx) :-> Invest
                    , Just (odd idx) :-> Disinvest ]

