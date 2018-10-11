

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec


import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal (Signal(..))

import Trade.Type.Impulse (Impulse(..))

type OptimizedImpulseGenerator ohlc = PriceSignal ohlc -> ImpulseSignal

type ImpulseGenerator inp ohlc = inp ohlc -> OptimizedImpulseGenerator ohlc

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
