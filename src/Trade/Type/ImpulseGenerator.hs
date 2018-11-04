{-# LANGUAGE RankNTypes #-}

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec

import qualified Trade.Type.OHLC as O

import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import qualified Trade.Type.Signal.Impulse as IS

import Trade.Type.Impulse (Impulse(..))

import qualified Trade.Timeseries.Algorithm.Intersection as Inter
import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Algorithm.MovingAverage as MAvg

newtype OptimizedImpulseGenerator ohlc = OptimizedImpulseGenerator {
  unOptimizedImpulseGenerator :: forall t. (Ord t) => Signal t ohlc -> ImpulseSignal t
  }

newtype ImpulseGenerator optData ohlc = ImpulseGenerator {
  unImpulseGenerator :: optData -> OptimizedImpulseGenerator ohlc
  }

optImpGen2impGen :: OptimizedImpulseGenerator ohlc -> ImpulseGenerator optData ohlc
optImpGen2impGen ig = ImpulseGenerator (\_ -> ig)

-- | Do nothing
noImpulses :: OptimizedImpulseGenerator ohlc
noImpulses =
  let f (Signal ps) = Signal (Vec.map (fmap (const Nothing)) ps)
  in OptimizedImpulseGenerator f
  

-- | Classic buy and hold
buyAndHold :: OptimizedImpulseGenerator ohlc
buyAndHold =
  let go (Signal ps) =
        let f 0 = Just Buy
            f i | i == Vec.length ps - 1 = Just Sell
            f _ = Nothing
        in Signal (Vec.imap (\i -> fmap (const (f i))) ps)
  in OptimizedImpulseGenerator go
  

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

-- | This impulse generator looks ahead in time, which is not possible in reality. It yields maximal profit.
optimalBuySell :: (Ord a) => (ohlc -> a) -> OptimizedImpulseGenerator ohlc
optimalBuySell trdAt =
  let go (Signal ps) =
        let qs = Vec.zipWith3 f ps (Vec.tail ps) (Vec.tail (Vec.tail ps))
            f (_, p0) (t1, p1) (_, p2)
              | trdAt p0 < trdAt p1 && trdAt p1 > trdAt p2 = (t1, Just Sell)
              | trdAt p0 > trdAt p1 && trdAt p1 < trdAt p2 = (t1, Just Buy)
              | otherwise = (t1, Nothing)
        
            (ti0, pr0) = ps Vec.! 0
            (_, pr1) = ps Vec.! 1
            x = case trdAt pr0 < trdAt pr1 of
                  True -> (ti0, Just Buy)
                  False -> (ti0, Nothing)

            (tn, _) = ps Vec.! (Vec.length ps - 1)
            y = (tn, Just Sell)

                        
        in Signal (Vec.snoc (Vec.cons x qs) y)
  in OptimizedImpulseGenerator go



-- | Construct impulses from crosses of one moving average with the ticker
-- Buy/Sell at 'perc' percent offset. (Uses closing price).

impulsesFromMovingAverage ::
  (OHLC.OHLCInterface ohlc) => Double -> Int -> OptimizedImpulseGenerator ohlc
impulsesFromMovingAverage perc windowSize =
  let go (Signal ps) =
        let f (t, x) = (t, O.unClose $ OHLC.ohlcClose x)
            qs = Vec.map f ps
            avgs = MAvg.mavgBar windowSize qs

            g (t, q) (_, a)
              | q >= (1+perc)*a = (t, Just Sell)
              | q <= (1-perc)*a = (t, Just Buy)
              | otherwise = (t, Nothing)
              
            rs = Vec.zipWith g qs avgs
        in IS.alternateBuySellKeepFirstOccurrence (Signal rs)
  in OptimizedImpulseGenerator go



-- | Constuct impulses with crossing of two moving averages
-- (Uses closing price).
impulsesFromTwoMovingAverages ::
  (OHLC.OHLCInterface ohlc) => Int -> Int -> OptimizedImpulseGenerator ohlc
impulsesFromTwoMovingAverages j k =
  let go (Signal ps) =
        let f (t, x) = (t, O.unClose $ OHLC.ohlcClose x)

            qs = Vec.map f ps

            avgJ = MAvg.mavgBar j qs
            avgK = MAvg.mavgBar k qs

            tradeSignal Inter.Down = Just Buy
            tradeSignal Inter.Up = Just Sell
            tradeSignal Inter.NoIntersection = Nothing

        in IS.toImpulseSignal (\_ _ -> tradeSignal) (Inter.intersection avgJ avgK)
  in OptimizedImpulseGenerator go



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

