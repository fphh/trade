

module Trade.Strategy.Process where


import Control.Monad.State (State, get, evalState)


import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set

import qualified Data.List as List

import qualified Data.Vector as Vec


import Trade.Type.DisInvest (DisInvest(..), InvestSignal(..))
import Trade.Type.Signal (Signal(..))

import Trade.Strategy.Algorithm (modifySignal)
import Trade.Strategy.Type (Signals(..), AlignedSignals(..), IndexedSignals(..), Index(..))



align :: (Ord t, Fractional x) => Signals sym t x -> AlignedSignals sym t x
align (Signals tickers) =
  let tmsTickers = fmap (Set.fromList . Vec.toList . Vec.map fst . unSignal) tickers
      allTimes = foldMap id tmsTickers
      tms = Map.foldr (\vs acc -> Set.intersection acc vs) allTimes tmsTickers
      alSigs = fmap (Vec.map snd . Vec.filter ((`Set.member` tms) . fst) . unSignal) tickers
      modSigs = Map.mapWithKey modifySignal alSigs
  in AlignedSignals (Vec.fromList (Set.toList tms)) modSigs

process ::
  (Ord t, Fractional x, Ord sym) =>
  State (IndexedSignals sym t x) [(sym, DisInvest)] -> State (Signals sym t x) (Map sym (InvestSignal t))
process frame = do
  st <- get
  let asigs = align st
      atms = alignedTimes asigs
      
      f (currentBS, acc) i t =
        let res = evalState frame (IndexedSignals (Index i) asigs)
        
            p (sym, bs) = maybe (bs == Invest) (bs /=) (Map.lookup sym currentBS)
            fres = filter p res

            -- g (sym, bs) acc = Map.insert sym bs acc
            -- newCurrentBS = List.foldr g currentBS fres
            
            newCurrentBS = List.foldr (uncurry Map.insert) currentBS fres

            h (sym, bs) = Map.alter (Just . maybe (Map.singleton t bs) (Map.insert t bs)) sym
            newAcc = List.foldr h acc fres

        in (newCurrentBS, newAcc)

      (_, xs) = Vec.ifoldl f (Map.empty, Map.empty) atms
    
  return (fmap InvestSignal xs)



run :: State (Signals sym t x) (Map sym (InvestSignal t)) -> Map sym (InvestSignal t)
run st = evalState st (Signals Map.empty)
