{-# LANGUAGE FlexibleInstances #-}

module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec

import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Experiment (Result(..), Input(..), Output(..), OutputPerSymbol(..))
import Trade.Type.Impulse (Impulse)
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Sample (SplitIndex(..), splitTime)

import Trade.Type.Signal (Timeseries)
import qualified Trade.Type.Signal as Signal

import Trade.Type.Strategy.Index (Index(..))
import Trade.Strategy.Type (AlignedSignals(..))


import Trade.Analysis.Report (BacktestData(..))

import Trade.Report.Basic (text)
import Trade.Report.Config (HtmlReader)
import qualified Trade.Report.Chart as Chart
import Trade.Report.ToReport (ToReport, toReport)




data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

noBacktest :: a -> b -> NoBacktestReport
noBacktest _ _ = NoBacktestReport

instance ToReport (BacktestData a NoBacktestReport) where
  toReport _ = text "No backtest done."



data Backtest ohlc = Backtest {
  btImpulseSignal :: Map UTCTime Impulse
  , btDeltaTradeList :: DeltaTradeList ohlc
  , btInputSignal :: Timeseries ohlc
  }

concatBT :: [Backtest ohlc] -> Backtest ohlc
concatBT [] = error "concatBT: empty list of backtests"
concatBT bs =
  let h (Backtest a (DeltaTradeList b) c) (Backtest x (DeltaTradeList y) z) =
        Backtest (Map.union a x) (DeltaTradeList (b++y)) (Signal.concat [c, z])  
  in List.foldr1 h bs


backtest ::
  (Ord sym) =>
  SplitIndex -> Result stgy sym ohlc -> Backtest ohlc
backtest spIdx@(SplitIndex idx) (Result inp (Output asigs out)) =
  let sym = symbol inp

      ops =
        case Map.lookup sym out of
          Nothing -> error "backtest: DeltaTradeList not found"
          Just xs -> xs

      as =
        case Map.lookup sym (inputSignals inp) of
          Nothing -> error "backtest: symbol not found"
          Just xs -> xs

      t = splitTime spIdx as
      i = Index idx

      DeltaTradeList dts = deltaTradeList ops

      tl = alignedTimes asigs

      impSig =
        Map.mapKeys ((tl Vec.!) . unIndex)
        $ Map.filterWithKey (\k _ -> k >= i)
        $ unImpulseSignal (impulseSignal ops)

  in Backtest {
    btImpulseSignal = impSig 
    , btDeltaTradeList = DeltaTradeList (filter ((>= t) . start) dts)
    , btInputSignal = snd (Signal.splitAt idx as)
    }

render :: Backtest ohlc -> HtmlReader ()
render (Backtest is dts ts) =
  let outSig = undefined -- Signal.adjust eqty timeLine (concatDeltaSignals stp eqty dts)
  in Chart.bt is outSig
