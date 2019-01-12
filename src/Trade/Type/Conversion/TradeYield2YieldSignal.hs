{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}


module Trade.Type.Conversion.TradeYield2YieldSignal where


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

-- import Trade.Type.Bars (Time, DeltaT, addT)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))
-- import Trade.Type.StepFunc (StepFunc)
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

nt2eqty :: Vector Equity -> TradeYield -> Vector Equity
nt2eqty eqty (TradeYield _ ny) =
  let f vs (Yield y) = vs `Vec.snoc` Equity (y * unEquity (slast "f" vs))
  in Vec.foldl' f eqty ny


tradeYield2yieldSignal ::
  (Show t, Time t, Num (DeltaT t), Show (DeltaT t)) =>
  StepFunc Yield -> Equity -> t -> DeltaT t -> TradeYieldList -> EquitySignal t

tradeYield2yieldSignal _ _ _ _ (TradeYieldList []) =
  error "normTrade2yieldSignal: no trades in list"
tradeYield2yieldSignal stepFunc eqty start dt (TradeYieldList nts) =
  let xs = map yieldAccordingToPosition nts
      ntsi = List.foldl' nt2eqty (Vec.singleton eqty) xs
      addDate i e = (start `addT` (fromIntegral i * dt), e)
  in Signal (Vec.imap addDate ntsi)
-}
