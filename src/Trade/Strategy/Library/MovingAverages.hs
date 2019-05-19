


module Trade.Strategy.Library.MovingAverages where

import Control.Monad (void)

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.DisInvest (DisInvest(..), InvestSignal)
import Trade.Type.Signal (Signal)
--import qualified Trade.Type.Signal as Signal

import Trade.Strategy.Type (Signals, AlignedSignals, Window(..))

import Trade.Strategy.Algorithm (now, mavg)
import Trade.Strategy.Condition (symbol, conditions, (.>), (.<), (.&&), Condition((:=:)), Implication((:->)))
import Trade.Strategy.Process (process)




movingAverages ::
  (Ord t, Ord sym, Ord x, Fractional x, Floating x) =>
  Window -> Window -> [(sym, Signal t x)] -> State (Signals sym t x) (AlignedSignals sym t x, Map sym (InvestSignal t))
movingAverages _ _ [] = error "movingAverages"
movingAverages j k (vs:_) = do

  void (now vs)

  vs10 <- mavg j vs
  vs20 <- mavg k vs

  process $ do

    v10_1 <- vs10 (-1)
    v10_0 <- vs10 0

    v20_1 <- vs20 (-1)
    v20_0 <- vs20 0

    conditions $
      
      symbol vs :=: [ v10_1 .> v20_1 .&& v10_0 .< v20_0 :-> Disinvest
                    , v10_1 .< v20_1 .&& v10_0 .> v20_0 :-> Invest ]
