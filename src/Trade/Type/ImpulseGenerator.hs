{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec

import qualified Data.Map as Map

import Trade.Type.Signal (Signal(..))

import Trade.Type.ImpulseSignal (ImpulseSignal(..))

import Trade.Type.Impulse (Impulse(..))
import qualified Trade.Type.Impulse as Imp

import qualified Trade.Timeseries.Algorithm.Intersection as Inter

import qualified Trade.Algorithm.MovingAverage as MAvg

import qualified Trade.Timeseries.Algorithm.SyncZip as SZ

newtype OptimizedImpulseGenerator ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator :: forall t. (Ord t) => Signal t ohlc -> ImpulseSignal t
  }

newtype ImpulseGenerator optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator ohlc
  }

newtype RankedStrategies ohlcDataTy = RankedStrategies {
  rankedStrategies :: [OptimizedImpulseGenerator ohlcDataTy]
  }

-- | Non empty list. We should replace it by some package.
data NonEmptyList a = NonEmptyList {
  head :: a
  , tail :: [a]
  }

toImpGen :: (optData -> (forall t. Ord t => Signal t ohlc -> ImpulseSignal t)) -> ImpulseGenerator optData ohlc
toImpGen ig = ImpulseGenerator (\optData -> OptimizedImpulseGenerator (ig optData))

optImpGen2impGen :: OptimizedImpulseGenerator ohlc -> ImpulseGenerator optData ohlc
optImpGen2impGen ig = ImpulseGenerator (\_ -> ig)


mapIG :: (Impulse -> Impulse) -> ImpulseGenerator optData ohlc -> ImpulseGenerator optData ohlc
mapIG f (ImpulseGenerator ig) =
  let newIS (ImpulseSignal m) = ImpulseSignal (fmap f m)
      newOIG (OptimizedImpulseGenerator oig) = OptimizedImpulseGenerator (\signal -> newIS (oig signal))
  in ImpulseGenerator (\d -> newOIG (ig d))

invert :: ImpulseGenerator optData ohlc -> ImpulseGenerator optData ohlc
invert = mapIG Imp.invert


-- 
-- TODO: Verify:
-- \f optData -> ((\(ImpulseGenerator ig) -> optImGen2impGen (ig optData) == toImpGen f)  (toImpGen f))
-- Is it true? Somehow... quantification of optData is not ok...
--

-- | Do nothing
noImpulses :: OptimizedImpulseGenerator ohlc
noImpulses = OptimizedImpulseGenerator (const (ImpulseSignal Map.empty))

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

{-
-- | Constuct impulses with crossing of two moving averages
impulsesFromTwoMovingAverages ::
  (ohlc -> Double) -> MAvg.WindowSize -> MAvg.WindowSize -> OptimizedImpulseGenerator ohlc
impulsesFromTwoMovingAverages trdAt (MAvg.WindowSize j) (MAvg.WindowSize k) =
  let go (Signal ps) =
        let qs = Vec.map (fmap trdAt) ps

            avgJ = MAvg.mavgBar j qs
            avgK = MAvg.mavgBar k qs

            g acc (t, Inter.Down) = Map.insert t Buy acc
            g acc (t, Inter.Up) = Map.insert t Sell acc
            g acc _ = acc

        in ImpulseSignal (Vec.foldl' g Map.empty (Inter.intersection avgJ avgK))
  in OptimizedImpulseGenerator go
-}


{-
-- | Buy after n times up, sell after m times down.
buySellAfterNM ::
  (OHLC.OHLCInterface ohlc) => Int -> Int -> OptimizedImpulseGenerator ohlc
buySellAfterNM b s =
  let go (Signal ps) =
        let f (_, x) = O.unClose (OHLC.ohlcClose x)
            qs = Vec.toList (Vec.map f ps)

            sell xs | length (take s xs) < s = map (const Nothing) xs 
            sell xs@(_:as) =
              let (y:ys, zs) = splitAt s xs
              in case all (<y) ys of
                   True -> map (const Nothing) ys ++ [Just Sell] ++ buy zs
                   False -> Nothing : sell as
            sell _ = error "ImpulseGenerator.buySellAfterNM: sell, never here"

            buy xs | length (take b xs) < b = map (const Nothing) xs
            buy xs@(_:as) =
              let (y:ys, zs) = splitAt b xs
              in case all (>y) ys of
                   True -> map (const Nothing) ys ++ [Just Buy] ++ sell zs
                   False -> Nothing : buy as
            buy _ = error "ImpulseGenerator.buySellAfterNM: buy, never here"

            g i x = (fst (ps Vec.! i), x)
            res = Vec.imap g (Vec.fromList (buy qs))

        in Signal res
  in OptimizedImpulseGenerator go

-}



  
{-
-- | Buy if index in signal is even, sell if odd
buySell :: OptimizedImpulseGenerator ohlc
buySell =
  let go (Signal ps) =
        let f i (t, _) =
              (\x -> (t, Just x)) $
              case even i of
                True -> Buy
                False -> Sell
        in Signal (Vec.imap f ps)
  in OptimizedImpulseGenerator go


-- | Buying, selling at absolute prices (using closing price).
buyAtSellAtAbs ::
  (OHLC.OHLCInterface ohlc) =>
  Double -> Double -> OptimizedImpulseGenerator ohlc
buyAtSellAtAbs buy sell =
  let go =
        let f (t, x) = (\u -> (t, u)) $
              case O.unClose (OHLC.ohlcClose x) of
                y | y > buy -> Just Buy 
                y | y < sell -> Just Sell
                _ -> Nothing
        in IS.alternateBuySellKeepFirstOccurrence . Signal.map f
  in OptimizedImpulseGenerator go
-}

