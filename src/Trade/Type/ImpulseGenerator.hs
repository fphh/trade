

module Trade.Type.ImpulseGenerator where

import qualified Data.Vector as Vec


import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal (Signal(..))

import Trade.Type.Impulse (Impulse(..))


type ImpulseGenerator inp = inp -> ImpulseSignal

noImpulses :: ImpulseGenerator (PriceSignal ohlc)
noImpulses (Signal ps) = Signal (Vec.map (fmap (const Nothing)) ps)

buyAndHold :: ImpulseGenerator (PriceSignal ohlc)
buyAndHold (Signal ps) =
  let f 0 = Just Buy
      f i | i == Vec.length ps - 1 = Just Sell
      f _ = Nothing
  in Signal (Vec.imap (\i -> fmap (const (f i))) ps)


buySell :: ImpulseGenerator (PriceSignal ohlc)
buySell (Signal ps) =
  let f i (t, _) =
        (\x -> (t, Just x)) $
              case even i of
                True -> Buy
                False -> Sell
  in Signal (Vec.imap f ps)

-- | This impulse generator looks ahead in time, which is not possible in reality. It yields maximal profit.
optimalBuySell :: (Ord a) => (ohlc -> a) -> ImpulseGenerator (PriceSignal ohlc)
optimalBuySell trdAt (Signal ps) =
  let qs = Vec.zipWith3 f ps (Vec.tail ps) (Vec.tail (Vec.tail ps))
      f (t0, p0) (t1, p1) (t2, p2)
        | trdAt p0 < trdAt p1 && trdAt p1 > trdAt p2 = (t1, Just Sell)
        | trdAt p0 > trdAt p1 && trdAt p1 < trdAt p2 = (t1, Just Buy)
        | otherwise = (t1, Nothing)
        
      (t0, p0) = ps Vec.! 0
      (t1, p1) = ps Vec.! 1
      x = case trdAt p0 < trdAt p1 of
            True -> (t0, Just Buy)
            False -> (t0, Nothing)

      (tn, _) = ps Vec.! (Vec.length ps - 1)
      y = (tn, Just Sell)

                        
  in Signal (Vec.snoc (Vec.cons x qs) y)
