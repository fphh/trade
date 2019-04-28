{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec

import qualified Data.Map as Map

import Trade.Type.Signal (Signal(..))

import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import qualified Trade.Type.ImpulseSignal as IS

import Trade.Type.Impulse (Impulse(..))
import qualified Trade.Type.Impulse as Imp

import qualified Trade.Timeseries.Algorithm.Intersection as Inter

import qualified Trade.Algorithm.MovingAverage as MAvg

import qualified Trade.Timeseries.Algorithm.SyncZip as SZ

newtype OptimizedImpulseGenerator stgy ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator :: forall t. (Ord t) => Signal t ohlc -> ImpulseSignal stgy t
  }

newtype ImpulseGenerator stgy optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator stgy ohlc
  }

newtype RankedStrategies stgy ohlcDataTy = RankedStrategies {
  rankedStrategies :: [OptimizedImpulseGenerator stgy ohlcDataTy]
  }


toImpGen :: (optData -> (forall t. Ord t => Signal t ohlc -> ImpulseSignal stgy t)) -> ImpulseGenerator stgy optData ohlc
toImpGen ig = ImpulseGenerator (\optData -> OptimizedImpulseGenerator (ig optData))

optImpGen2impGen :: OptimizedImpulseGenerator stgy ohlc -> ImpulseGenerator stgy optData ohlc
optImpGen2impGen ig = ImpulseGenerator (\_ -> ig)

{-

mapIG :: (Impulse -> Impulse) -> ImpulseGenerator optData ohlc -> ImpulseGenerator optData ohlc
mapIG f (ImpulseGenerator ig) =
  let newIS (ImpulseSignal m) = ImpulseSignal (fmap f m)
      newOIG (OptimizedImpulseGenerator oig) = OptimizedImpulseGenerator (\signal -> newIS (oig signal))
  in ImpulseGenerator (\d -> newOIG (ig d))

invert :: ImpulseGenerator stgy optData ohlc -> ImpulseGenerator stgy optData ohlc
invert = mapIG Imp.invert

invertOpt :: OptimizedImpulseGenerator ohlc -> OptimizedImpulseGenerator ohlc
invertOpt (OptimizedImpulseGenerator ig) = OptimizedImpulseGenerator (fmap IS.invert ig)

-- 
-- TODO: Verify:
-- \f optData -> ((\(ImpulseGenerator ig) -> optImGen2impGen (ig optData) == toImpGen f)  (toImpGen f))
-- Is it true? Somehow... quantification of optData is not ok...
--
-}

-- | Do nothing
noImpulses :: OptimizedImpulseGenerator stgy ohlc
noImpulses = OptimizedImpulseGenerator (const (ImpulseSignal Map.empty))

{-
-- | Classic buy and hold
buyAndHold :: OptimizedImpulseGenerator ohlc
buyAndHold =
  let go (Signal ps) =
        let (t0, _) = Vec.head ps
            (tn, _) = Vec.last ps
        in ImpulseSignal (Map.fromList [(t0, Buy), (tn, Sell)])
  in OptimizedImpulseGenerator go

-- | This impulse generator looks ahead in time, which is not possible in reality. It yields maximal profit.
optimalBuySell :: (Ord a) => (ohlc -> a) -> OptimizedImpulseGenerator ohlc
optimalBuySell trdAt =
  let go (Signal ps) =
        let f m ((_, p0), (t1, p1), (_, p2))
              | trdAt p0 < trdAt p1 && trdAt p1 >= trdAt p2 = Map.insert t1 Sell m
              | trdAt p0 > trdAt p1 && trdAt p1 <= trdAt p2 = Map.insert t1 Buy m
              | otherwise = m
                
            m0 = Vec.foldl' f Map.empty (Vec.zip3 ps (Vec.tail ps) (Vec.tail (Vec.tail ps)))

            (ti0, pr0) = ps Vec.! 0
            (_, pr1) = ps Vec.! 1
            m1 = case trdAt pr0 < trdAt pr1 of
                  True -> Map.insert ti0 Buy m0
                  False -> m0

        in ImpulseSignal m1
  in OptimizedImpulseGenerator go


newtype Percent = Percent Double deriving (Show)

-- | Construct impulses from crosses of one moving average with the ticker
-- Buy/Sell at 'perc' percent offset. Mean reversion?
impulsesFromMovingAverage ::
  (ohlc -> Double) -> ImpulseGenerator (Percent, MAvg.WindowSize) ohlc
impulsesFromMovingAverage trdAt =
  let go (Percent perc, winSize) (Signal ps) =
        let qs = Vec.map (fmap trdAt) ps
            avgs = MAvg.mavgBar winSize qs

            g acc (t, (q, a))
              | q <= (1+perc)*a = Map.insert t Buy acc
              | q >= (1-perc)*a = Map.insert t Sell acc
              | otherwise = acc

            ss = Vec.foldl' g Map.empty (SZ.syncZip qs avgs)
            
        in ImpulseSignal ss
        
  in toImpGen go


-- | Constuct impulses with crossing of two moving averages
impulsesFromTwoMovingAverages ::
  (ohlc -> Double) -> ImpulseGenerator (MAvg.WindowSize, MAvg.WindowSize) ohlc
impulsesFromTwoMovingAverages trdAt =
  let go (j, k) (Signal ps) =
        let qs = Vec.map (fmap trdAt) ps

            avgJ = MAvg.mavgBar j qs
            avgK = MAvg.mavgBar k qs

            g acc (t, Inter.Down) = Map.insert t Buy acc
            g acc (t, Inter.Up) = Map.insert t Sell acc
            g acc _ = acc

        in ImpulseSignal (Vec.foldl' g Map.empty (Inter.intersection avgJ avgK))
        
  in toImpGen go


-}
