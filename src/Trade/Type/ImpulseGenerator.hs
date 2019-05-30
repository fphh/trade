{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import Control.Monad.State (State)

import Data.Map (Map)

import Trade.Type.DisInvest (InvestSignal(..))

import Trade.Type.Signal (Signal(..))

import Trade.Strategy.Type (Signals, AlignedSignals)


newtype OptimizedImpulseGenerator ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator ::
      forall t sym. (Ord sym, Ord t) =>
      Map sym (Signal t ohlc) -> State (Signals sym t ohlc) (AlignedSignals sym t ohlc, Map sym InvestSignal)
  }


newtype ImpulseGenerator optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator ohlc
  }


newtype RankedStrategies ohlc = RankedStrategies {
  rankedStrategies :: [OptimizedImpulseGenerator ohlc]
  }

