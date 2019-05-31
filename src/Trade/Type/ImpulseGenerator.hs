{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.DisInvest (InvestSignal(..))

import Trade.Type.Signal (Timeseries)

import Trade.Strategy.Type (Signals, AlignedSignals)


newtype OptimizedImpulseGenerator ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator ::
      forall sym. (Ord sym) =>
      Map sym (Timeseries ohlc) -> State (Signals sym ohlc) (AlignedSignals sym ohlc, Map sym InvestSignal)
  }


newtype ImpulseGenerator optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator ohlc
  }


newtype RankedStrategies ohlc = RankedStrategies {
  rankedStrategies :: [OptimizedImpulseGenerator ohlc]
  }

