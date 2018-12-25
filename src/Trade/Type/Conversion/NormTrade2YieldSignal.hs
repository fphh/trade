{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}


module Trade.Type.Conversion.NormTrade2YieldSignal where

import qualified Data.Vector as Vec
import qualified Data.List as List

import Trade.Type.Bars (Time, DeltaT, add)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))
import Trade.Type.StepFunc (StepFunc)
import Trade.Type.Yield (NoYield, noYield, Yield(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Signal.Equity (EquitySignal(..))


import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Debug.Trace

-- | TODO: check wether correct
yieldAccordingToPosition :: (NoYield yield) => NormTrade yield t -> NormTrade yield t
yieldAccordingToPosition (NormTrade NoPosition t vs) =
  NormTrade NoPosition t (Vec.replicate (Vec.length vs) noYield)
yieldAccordingToPosition (NormTrade state t vs) =
  NormTrade state t vs

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


normTrade2yieldSignal ::
  (Time t, Num (DeltaT t), NoYield yield, Show (DeltaT t), Show yield) =>
  StepFunc yield -> Equity -> t -> DeltaT t -> NormTradeList yield t -> EquitySignal t

normTrade2yieldSignal _ _ _ _ (NormTradeList []) =
  error "normTrade2yieldSignal: no trades in list"
normTrade2yieldSignal stepFunc eqty start dt (NormTradeList (nt:nts)) =
  let xs = map yieldAccordingToPosition nts

      g (t, Equity e) (Yield y) = (t, Equity (e*y))

      ntNew =
        case nt of
          NormTrade NoPosition dt ny -> Vec.scanl g (start, eqty) ny

                                        
  {-
      f :: Signal t Equity -> NormTrade yield t -> Signal t Equity
      f sig (NormTrade NoPosition _ vs) =
        let (tn, en) = Signal.last sig
            g i (t, _) = (tn `add` (fromIntegral i * dt), en)
            
        in undefined {- sig <> Signal.imap f (Signal (Vec.tail vs)) -}

        -}

      f sig (NormTrade LongPosition _ vs) =
        let (tn, en) = Signal.last sig
        in undefined
     

  in Signal (mconcat [ntNew])


    -- (List.scanl f (Signal.singleton (start, eqty)) xs)


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
