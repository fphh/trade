{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import Control.Monad.Trans.State (State)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec


import Trade.Type.DisInvest (InvestSignal(..))

import Trade.Type.Signal (Timeseries)

import Trade.Strategy.Type (Signals, AlignedSignals(..))


newtype OptimizedImpulseGenerator ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator ::
      forall sym. (Ord sym) =>
      sym -> Map sym (Timeseries ohlc) -> State (Signals sym ohlc) (AlignedSignals sym ohlc, Map sym InvestSignal)
  }


newtype ImpulseGenerator optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator ohlc
  }


newtype RankedStrategies ohlc = RankedStrategies {
  rankedStrategies :: [OptimizedImpulseGenerator ohlc]
  }


noImpulses :: ImpulseGenerator optData ohlc
noImpulses = ImpulseGenerator $
  \_ -> OptimizedImpulseGenerator $
        \_ _ -> return (AlignedSignals Vec.empty Map.empty, Map.empty)


