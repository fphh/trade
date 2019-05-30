

module Trade.Strategy.Process where


import Control.Monad.State (State, get, runState, evalState)


import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.List as List

import qualified Data.Vector as Vec


import Trade.Type.DisInvest (DisInvest(..), InvestSignal(..))

import Trade.Type.Add (Add)
import Trade.Type.Scale (Scale)

import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal (Signal(..))
import Trade.Type.Strategy.Index (Index(..))

import Trade.Statistics.Algorithm (Statistics)

import Trade.Strategy.Algorithm (modifySignal)
import Trade.Strategy.Type (Signals(..), AlignedSignals(..), IndexedSignals(..), Modified)

-- import Debug.Trace

data Resample =
  Upsample
  | Downsample

data ResampleFunc k t x = ResampleFunc {
  sampleMethod :: Set t -> Signal t x -> Signal t x
  , timelineMethod :: Set t -> Map k (Set t) -> Set t
  }

resample :: (Ord t) => Resample -> ResampleFunc k t x
resample Upsample = ResampleFunc Signal.upsample const
resample Downsample = ResampleFunc Signal.downsample (Map.foldr (\vs acc -> Set.intersection acc vs))


align ::
  (Ord t, Statistics x, Add x, Scale x) =>
  ResampleFunc (Modified sym) t x -> Signals sym t x -> AlignedSignals sym t x
align (ResampleFunc sampleMeth timelineMeth) (Signals tickers) =
  let tmsTickers = fmap (Set.fromList . Vec.toList . Vec.map fst . unSignal) tickers
      allTimes = foldMap id tmsTickers
      tms = timelineMeth allTimes tmsTickers
      alSigs = fmap (Vec.map snd . unSignal . sampleMeth tms) tickers
      modSigs = Map.mapWithKey modifySignal alSigs
  in AlignedSignals (Vec.fromList (Set.toList tms)) modSigs

process ::
  (Ord t, Ord sym, Statistics x, Add x, Scale x) =>
  State (IndexedSignals sym t x) [(sym, DisInvest)]
  -> State (Signals sym t x) (AlignedSignals sym t x, Map sym InvestSignal)
process frame = do
  st <- get
  let asigs = align (resample Downsample) st
      atms = alignedTimes asigs
      
      f (currentBS, acc) i t =
        let idx = Index i
            res = evalState frame (IndexedSignals idx asigs)
        
            p (sym, bs) = maybe (bs == Invest) (bs /=) (Map.lookup sym currentBS)
            fres = filter p res

            newCurrentBS = List.foldr (uncurry Map.insert) currentBS fres

            -- h (sym, bs) = Map.alter (Just . maybe (Map.singleton t bs) (Map.insert t bs)) sym
            h (sym, bs) = Map.alter (Just . maybe (Map.singleton idx bs) (Map.insert idx bs)) sym

            newAcc = List.foldr h acc fres

        in (newCurrentBS, newAcc)

      (_, xs) = Vec.ifoldl f (Map.empty, Map.empty) atms
    
  return (asigs, fmap InvestSignal xs)



run ::
  State (Signals sym t x) (AlignedSignals sym t x, Map sym InvestSignal)
  -> ((AlignedSignals sym t x, Map sym InvestSignal), Signals sym t x)
run st = runState st (Signals Map.empty)
