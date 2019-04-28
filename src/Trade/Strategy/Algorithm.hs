

module Trade.Strategy.Algorithm where

import Control.Monad.State (State, get, modify)

import qualified Data.Map as Map


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Type.Signal (Signal(..))

import Trade.Strategy.Type (Index(..), Focus(..), Offset(..), Modified(..), Signals(..), AlignedSignals(..), IndexedSignals(..))


apply :: Offset -> Index -> (Index -> Focus, Vector x) -> Maybe x
apply (Offset off) idx (f, vs) =
  let Focus foc = f idx
  in vs Vec.!? (foc+off)

indicator ::
  (Ord sym) =>
  (Modified sym) -> Signal t x -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
indicator mSym vs = do
  modify (\st -> st { signals = Map.insert mSym vs (signals st) })
  return $ \off -> do
    IndexedSignals idx ast <- get
    let sig = modifiedSignals ast Map.! mSym
    return (apply off idx sig)

time :: Offset -> State (IndexedSignals sym t x) (Maybe t)
time (Offset off) = do
  IndexedSignals (Index idx) ast <- get
  let tms = alignedTimes ast
      k = idx+off
  return (tms Vec.!? k)




modifySignal :: (Fractional x) => (Modified sym) -> Vector x -> (Index -> Focus, Vector x)
modifySignal (Now _) vs = (\(Index idx) -> Focus idx, vs)
modifySignal (MAvg win _) vs = 
  let winLen = fromIntegral win
      us = Vec.imap (\i _ -> (Vec.sum (Vec.slice i win vs) / winLen)) vs
  in (\(Index idx) -> Focus (idx-win+1), Vec.slice 0 (Vec.length vs - win + 1) us)


now ::
  (Ord sym) =>
  (sym, Signal t x) -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
now (sym, vs) = indicator (Now sym) vs

mavg ::
  (Ord sym) =>
  Int -> (sym, Signal t x) -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
mavg win (sym, vs) = indicator (MAvg win sym) vs
