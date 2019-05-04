

module Trade.Strategy.Algorithm where

import Control.Monad (liftM)

import Control.Monad.State (State, get, modify)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Type.Signal (Signal(..))

import Trade.Strategy.Type (Index(..), Focus(..), Offset(..), Modified(..), Signals(..), AlignedSignals(..), IndexedSignals(..), Window(..), K(..))


apply :: Offset -> Index -> (Index -> Focus, Vector x) -> Maybe x
apply (Offset off) idx (f, vs) =
  let Focus foc = f idx
  in vs Vec.!? (foc+off)


alignedSignals2signals :: (Eq t, Eq x) => AlignedSignals sym t x -> Map (Modified sym) (Maybe (Vector (t, x)))
alignedSignals2signals (AlignedSignals tms m) =
  let f vs =
        let g i t = sequence (t, apply (Offset 0) (Index i) vs)
        in sequence (Vec.filter (/= Nothing) (Vec.imap g tms))
  in fmap f m

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

start :: State (IndexedSignals sym t x) (Maybe t)
start = do
  IndexedSignals _ ast <- get
  let tms = alignedTimes ast
  return (tms Vec.!? 0)


end :: State (IndexedSignals sym t x) (Maybe t)
end = do
  IndexedSignals _ ast <- get
  let tms = alignedTimes ast
  return (tms Vec.!? (Vec.length tms - 1))





modifySignal :: (Fractional x, Floating x) => (Modified sym) -> Vector x -> (Index -> Focus, Vector x)
modifySignal (Now _) vs = (\(Index idx) -> Focus idx, vs)
modifySignal (MAvg (Window win) _) vs = 
  let winLen = fromIntegral win
      us = Vec.imap (\i _ -> (Vec.sum (Vec.slice i win vs) / winLen)) vs
  in (\(Index idx) -> Focus (idx-win+1), Vec.slice 0 (Vec.length vs - win + 1) us)
modifySignal (StdDev (Window win) (K k) _) vs =
  let winLen = fromIntegral win
      f i _ =
        let ws = Vec.slice i win vs
            mean = Vec.sum ws / winLen
            s = sqrt (Vec.sum (Vec.map ((**2) . (+ negate mean)) ws) / winLen)
        in mean + fromRational (toRational k) * s
            
      us = Vec.imap f vs
      
  in (\(Index idx) -> Focus (idx-win+1), Vec.slice 0 (Vec.length vs - win + 1) us)


now ::
  (Ord sym) =>
  (sym, Signal t x) -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
now (sym, vs) = indicator (Now sym) vs

mavg ::
  (Ord sym) =>
  Window -> (sym, Signal t x) -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
mavg win (sym, vs) = indicator (MAvg win sym) vs

stdDev ::
  (Ord sym) =>
  Window -> K -> (sym, Signal t x) -> State (Signals sym t x) (Offset -> State (IndexedSignals sym t x) (Maybe x))
stdDev win k (sym, vs) = indicator (StdDev win k sym) vs


