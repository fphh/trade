{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}


module Trade.Type.Conversion.TradeYield2YieldSignal where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import Trade.Type.Bars (Time, DeltaT, add)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Yield (NoYield, noYield, Yield(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Signal.Equity (EquitySignal(..))


import Trade.Type.TradeYield (TradeYield(..), TradeYieldList(..))

import Trade.Help.SafeTail (slast, sinit, stail)

import Debug.Trace

-- | TODO: check wether correct
yieldAccordingToPosition :: TradeYield -> TradeYield
yieldAccordingToPosition (TradeYield NoPosition vs) =
  TradeYield NoPosition (Vec.replicate (Vec.length vs) noYield)
yieldAccordingToPosition (TradeYield state vs) =
  TradeYield state vs

{-
-- | TODO: check wether correct
normTrade2yieldSignal ::
  (Time t, Num (DeltaT t), NoYield yield, Show (DeltaT t), Show yield) =>
  t -> DeltaT t -> NormTradeList yield t -> Signal t yield
normTrade2yieldSignal start dt (NormTradeList nts) = trace (show nts) $
  let xs = map (normedYield . yieldAccordingToPosition) nts
      f i x = (start `add` (fromIntegral i * dt), x)
      sig = Vec.imap f (Vec.cons noYield (Vec.concat xs))
  in Signal sig
-}



nt2eqty :: Vector Equity -> TradeYield -> Vector Equity
nt2eqty eqty (TradeYield _ ny) =
  let f vs (Yield y) = vs `Vec.snoc` Equity (y * unEquity (slast "f" vs))
      res = Vec.foldl' f eqty (stail "stail" ny)
  in trace (show (Vec.length ny) ++ " " ++ show (Vec.length res)) res


tradeYield2yieldSignal ::
  (Show t, Time t, Num (DeltaT t), Show (DeltaT t)) =>
  StepFunc Yield -> Equity -> t -> DeltaT t -> TradeYieldList -> EquitySignal t

tradeYield2yieldSignal _ _ _ _ (TradeYieldList []) =
  error "normTrade2yieldSignal: no trades in list"
tradeYield2yieldSignal stepFunc eqty start dt (TradeYieldList nts) = trace (show start) $ 
  let xs = map yieldAccordingToPosition nts

      f acc nt = acc Vec.++ nt2eqty acc nt

      ntsi = List.foldl' f (Vec.singleton eqty) xs


  in Signal (Vec.imap (\i e ->  (start `add` (fromIntegral i * dt), e)) ntsi)


{-

[
  NormTrade {normTradePosition = LongPosition, normTradeDuration = 691200s, normedYield = [Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 1209600s, normedYield = [Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 1.25},Yield {unYield = 1.2},Yield {unYield = 1.1666666666666667},Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 1468800s, normedYield = [Yield {unYield = 1.25},Yield {unYield = 1.2},Yield {unYield = 1.1666666666666667},Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647},Yield {unYield = 1.0555555555555556},Yield {unYield = 1.0526315789473684},Yield {unYield = 1.05}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 1209600s, normedYield = [Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 691200s, normedYield = [Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0},Yield {unYield = 1.0}]}]

-}



{-

[
  NormTrade {normTradePosition = LongPosition, normTradeDuration = 691200s, normedYield = [Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 1209600s, normedYield = [Yield {unYield = 0.9444444444444444},Yield {unYield = 0.9411764705882353},Yield {unYield = 0.9375},Yield {unYield = 0.9333333333333333},Yield {unYield = 0.9285714285714286},Yield {unYield = 0.9230769230769231},Yield {unYield = 0.9166666666666666},Yield {unYield = 0.9090909090909091},Yield {unYield = 0.9},Yield {unYield = 0.8888888888888888},Yield {unYield = 0.875},Yield {unYield = 0.8571428571428571},Yield {unYield = 0.8333333333333334},Yield {unYield = 0.8}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 1.25},Yield {unYield = 1.2},Yield {unYield = 1.1666666666666667},Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 0.9333333333333333},Yield {unYield = 0.9285714285714286},Yield {unYield = 0.9230769230769231},Yield {unYield = 0.9166666666666666},Yield {unYield = 0.9090909090909091},Yield {unYield = 0.9},Yield {unYield = 0.8888888888888888},Yield {unYield = 0.875},Yield {unYield = 0.8571428571428571},Yield {unYield = 0.8333333333333334},Yield {unYield = 0.8}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 1468800s, normedYield = [Yield {unYield = 1.25},Yield {unYield = 1.2},Yield {unYield = 1.1666666666666667},Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647},Yield {unYield = 1.0555555555555556},Yield {unYield = 1.0526315789473684},Yield {unYield = 1.05}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 1209600s, normedYield = [Yield {unYield = 0.9523809523809523},Yield {unYield = 0.95},Yield {unYield = 0.9473684210526315},Yield {unYield = 0.9444444444444444},Yield {unYield = 0.9411764705882353},Yield {unYield = 0.9375},Yield {unYield = 0.9333333333333333},Yield {unYield = 0.9285714285714286},Yield {unYield = 0.9230769230769231},Yield {unYield = 0.9166666666666666},Yield {unYield = 0.9090909090909091},Yield {unYield = 0.9},Yield {unYield = 0.8888888888888888},Yield {unYield = 0.875}]},

  NormTrade {normTradePosition = LongPosition, normTradeDuration = 950400s, normedYield = [Yield {unYield = 1.1428571428571428},Yield {unYield = 1.125},Yield {unYield = 1.1111111111111112},Yield {unYield = 1.1},Yield {unYield = 1.0909090909090908},Yield {unYield = 1.0833333333333333},Yield {unYield = 1.0769230769230769},Yield {unYield = 1.0714285714285714},Yield {unYield = 1.0666666666666667},Yield {unYield = 1.0625},Yield {unYield = 1.0588235294117647}]},

  NormTrade {normTradePosition = NoPosition, normTradeDuration = 691200s, normedYield = [Yield {unYield = 0.9444444444444444},Yield {unYield = 0.9411764705882353},Yield {unYield = 0.9375},Yield {unYield = 0.9333333333333333},Yield {unYield = 0.9285714285714286},Yield {unYield = 0.9230769230769231},Yield {unYield = 0.9166666666666666},Yield {unYield = 0.9090909090909091}]}

  ]

-}
