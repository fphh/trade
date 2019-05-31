

module Trade.Strategy.Algorithm where


import Control.Monad.State (State, get, modify)

import Data.Time.Clock (UTCTime)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Type.Add (Add, add)
import Trade.Type.Scale (Scale, scale)
import Trade.Type.Signal (Timeseries, Signal(..))
import Trade.Type.Strategy.Index (Index(..))

import Trade.Statistics.Algorithm (Statistics)
import qualified Trade.Statistics.Algorithm as SA

import Trade.Strategy.Type (Focus(..), Offset(..), Modified(..), Signals(..), AlignedSignals(..), IndexedSignals(..), Window(..), K(..))


apply :: Offset -> p -> (p -> Focus, Vector a) -> Maybe a
apply (Offset off) idx (f, vs) =
  let Focus foc = f idx
  in vs Vec.!? (foc+off)


alignedSignals2signals :: (Eq x) => AlignedSignals sym x -> Map (Modified sym) (Maybe (Vector (UTCTime, x)))
alignedSignals2signals (AlignedSignals tms m) =
  let f vs =
        let g i t = sequence (t, apply (Offset 0) (Index i) vs)
        in sequence (Vec.filter (/= Nothing) (Vec.imap g tms))
  in fmap f m

indicator ::
  (Ord sym) =>
  (Modified sym) -> Timeseries x -> State (Signals sym x) (Offset -> State (IndexedSignals sym x) (Maybe x))
indicator mSym vs = do
  modify (\st -> st { signals = Map.insert mSym vs (signals st) })
  return $ \off -> do
    IndexedSignals idx ast <- get
    let sig = modifiedSignals ast Map.! mSym
    return (apply off idx sig)

time :: Offset -> State (IndexedSignals sym x) (Maybe UTCTime)
time (Offset off) = do
  IndexedSignals (Index idx) ast <- get
  let tms = alignedTimes ast
      k = idx+off
  return (tms Vec.!? k)

start :: State (IndexedSignals sym x) (Maybe UTCTime)
start = do
  IndexedSignals _ ast <- get
  let tms = alignedTimes ast
  return (tms Vec.!? 0)


end :: State (IndexedSignals sym x) (Maybe UTCTime)
end = do
  IndexedSignals _ ast <- get
  let tms = alignedTimes ast
  return (tms Vec.!? (Vec.length tms - 1))

index :: State (IndexedSignals sym x) Index
index = do
  IndexedSignals idx _ <- get
  return idx



modifySignal :: (Statistics x, Add x, Scale x) => (Modified sym) -> Vector x -> (Index -> Focus, Vector x)
modifySignal (Now _) vs = (\(Index idx) -> Focus idx, vs)

modifySignal (MAvg (Window win) _) vs = 
  let us = Vec.imap (\i _ -> SA.mean (Vec.slice i win vs)) vs
  in (\(Index idx) -> Focus (idx-win+1), Vec.slice 0 (Vec.length vs - win + 1) us)

modifySignal (StdDev (Window win) (K k) _) vs =
  let f i _ =
        let ws = Vec.slice i win vs
        in SA.mean ws `add` scale k (SA.stdDev ws)
      us = Vec.imap f vs
  in (\(Index idx) -> Focus (idx-win+1), Vec.slice 0 (Vec.length vs - win + 1) us)



now ::
  (Ord sym) =>
  (sym, Timeseries x) -> State (Signals sym x) (Offset -> State (IndexedSignals sym x) (Maybe x))
now (sym, vs) = indicator (Now sym) vs

mavg ::
  (Ord sym) =>
  Window -> (sym, Timeseries x) -> State (Signals sym x) (Offset -> State (IndexedSignals sym x) (Maybe x))
mavg win (sym, vs) = indicator (MAvg win sym) vs

stdDev ::
  (Ord sym) =>
  Window -> K -> (sym, Timeseries x) -> State (Signals sym x) (Offset -> State (IndexedSignals sym x) (Maybe x))
stdDev win k (sym, vs) = indicator (StdDev win k sym) vs


