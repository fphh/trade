

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec

import qualified Trade.Type.OHLC as O

import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal (Signal(..))

import qualified Trade.Type.Signal.Impulse as IS

import Trade.Type.Impulse (Impulse(..))

import qualified Trade.Timeseries.Algorithm.Intersection as Inter
import qualified Trade.Timeseries.OHLC as OHLC

import qualified Trade.Algorithm.MovingAverage as MAvg


type OptimizedImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal

type ImpulseGenerator optInput ohlc = optInput ohlc -> OptimizedImpulseGenerator ohlc

optImpGen2impGen :: OptimizedImpulseGenerator ohlc -> ImpulseGenerator inp ohlc
optImpGen2impGen ig = \_ -> ig

noImpulses :: OptimizedImpulseGenerator ohlc
noImpulses (Signal ps) = Signal (Vec.map (fmap (const Nothing)) ps)

buyAndHold :: OptimizedImpulseGenerator ohlc
buyAndHold (Signal ps) =
  let f 0 = Just Buy
      f i | i == Vec.length ps - 1 = Just Sell
      f _ = Nothing
  in Signal (Vec.imap (\i -> fmap (const (f i))) ps)


-- | Buy if index in signal is even, sell if odd
buySell :: OptimizedImpulseGenerator ohlc
buySell (Signal ps) =
  let f i (t, _) =
        (\x -> (t, Just x)) $
              case even i of
                True -> Buy
                False -> Sell
  in Signal (Vec.imap f ps)

-- | This impulse generator looks ahead in time, which is not possible in reality. It yields maximal profit.
optimalBuySell :: (Ord a) => (ohlc -> a) -> OptimizedImpulseGenerator ohlc
optimalBuySell trdAt (Signal ps) =
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

-- | Constuct impulses with crossing of two moving averages
impulsesFromMovingAverages ::
  (OHLC.OHLCInterface ohlc) => Int -> Int -> OptimizedImpulseGenerator ohlc
impulsesFromMovingAverages j k (Signal ps) =
  let f (t, x) = (t, O.unClose $ OHLC.ohlcClose x)

      qs = Vec.map f ps

      avgJ = MAvg.mavgTime j qs
      avgK = MAvg.mavgTime k qs

      tradeSignal Inter.Down = Just Buy
      tradeSignal Inter.Up = Just Sell
      tradeSignal Inter.NoIntersection = Nothing

  in IS.toImpulseSignal (\_ _ -> tradeSignal) (Inter.intersection avgJ avgK)


-- | Buy after n times up, sell after m times down.
buySellAfterNM ::
  (OHLC.OHLCInterface ohlc) => Int -> Int -> OptimizedImpulseGenerator ohlc
buySellAfterNM b s (Signal ps) =
  let f (t, x) = O.unClose (OHLC.ohlcClose x)
      qs = Vec.toList (Vec.map f ps)

      sell xs | length (take s xs) < s = map (const Nothing) xs 
      sell xs@(_:as) =
        let (y:ys, zs) = splitAt s xs
        in case all (<y) ys of
             True -> map (const Nothing) ys ++ [Just Sell] ++ buy zs
             False -> Nothing : sell as

      buy xs | length (take b xs) < b = map (const Nothing) xs
      buy xs@(_:as) =
        let (y:ys, zs) = splitAt b xs
        in case all (>y) ys of
             True -> map (const Nothing) ys ++ [Just Buy] ++ sell zs
             False -> Nothing : buy as
             
      g i x = (fst (ps Vec.! i), x)
      res = Vec.imap g (Vec.fromList (buy qs))

  in Signal res
