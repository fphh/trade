


module Trade.Strategy.Library.MovingAverages where

import Control.Monad (void)

import Control.Monad.Trans.State (State)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Add (Add)
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.Scale (Scale)

import Trade.Type.DisInvest (DisInvest(..), InvestSignal)
import Trade.Type.Signal (Timeseries)
--import qualified Trade.Type.Signal as Signal

import Trade.Strategy.Type (Signals, AlignedSignals, Window(..), K(..))

import Trade.Strategy.Algorithm (now, mavg, stdDev)
import Trade.Strategy.Condition (symbol, conditions, (.>), (.<), (.&&), Condition((:=:)), Implication((:->)))
import Trade.Strategy.Process (process)

import Trade.Statistics.Algorithm (Statistics)


movingAverages ::
  (Ord x, Statistics x, Scale x, Add x) =>
  Window -> Window -> OptimizedImpulseGenerator x
movingAverages j k = OptimizedImpulseGenerator $ \sym ms -> do
  let vs = (sym, ms Map.! sym)
  
  void (now vs)

  vs10 <- mavg j vs
  vs20 <- mavg k vs

  process $ do

    v10_1 <- vs10 (-1)
    v10_0 <- vs10 0

    v20_1 <- vs20 (-1)
    v20_0 <- vs20 0

    conditions $
      
       sym :=: [ v10_1 .> v20_1 .&& v10_0 .< v20_0 :-> Disinvest
               , v10_1 .< v20_1 .&& v10_0 .> v20_0 :-> Invest ]




{-

movingAverages ::
  (Ord sym, Ord x, Statistics x, Scale x, Add x) =>
  Window -> Window -> sym -> Map sym (Timeseries x) -> State (Signals sym x) (AlignedSignals sym x, Map sym InvestSignal)
  
movingAverages _ _ _ ms | Map.null ms = error "movingAverages"
movingAverages j k sym ms = do

  let vs = (sym, ms Map.! sym)
  
  void (now vs)

  vs10 <- mavg j vs
  vs20 <- mavg k vs

  process $ do

    v10_1 <- vs10 (-1)
    v10_0 <- vs10 0

    v20_1 <- vs20 (-1)
    v20_0 <- vs20 0

    conditions $
      
       sym :=: [ v10_1 .> v20_1 .&& v10_0 .< v20_0 :-> Disinvest
               , v10_1 .< v20_1 .&& v10_0 .> v20_0 :-> Invest ]



-}





stdBreakout ::
  (Ord sym, Ord x, Statistics x, Scale x, Add x) =>
  Window -> K -> [(sym, Timeseries x)] -> State (Signals sym x) (AlignedSignals sym x, Map sym InvestSignal)
stdBreakout _ _ [] = error "stdBreakout"
stdBreakout j (K n) (vs:_) = do

  nw <- now vs

  void (mavg j vs)
  -- vs20 <- mavg k vs

  stdVSm <- stdDev j (K (-n)) vs
  
  stdVSp <- stdDev j (K n) vs

  process $ do

    nw_1 <- nw (-1)
    nw_0 <- nw 0

    m_1 <- stdVSm (-1)
    m_0 <- stdVSm 0

    p_1 <- stdVSp (-1)
    p_0 <- stdVSp 0



    conditions $
      
      symbol vs :=: [ nw_1 .< p_1 .&& nw_0 .> p_0 :-> Disinvest
                    , nw_1 .> m_1 .&& nw_0 .< m_0 :-> Invest ]
