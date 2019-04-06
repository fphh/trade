

module Trade.Type.Conversion.Impulse2TradeList where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Trade.Type.Position (Position(..))

import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Impulse (Impulse(..))

import Trade.Type.Strategy (Long, Short)

import Trade.Help.SafeTail (shead, stail)

import Debug.Trace


class Impulse2TradeList stgy where
  impulse2tradeList :: (Ord t) => Signal t ohlc -> ImpulseSignal t -> TradeList stgy t ohlc


longTag :: Impulse -> Position
longTag Buy = Invested
longTag Sell = NotInvested

shortTag :: Impulse -> Position
shortTag Buy = NotInvested
shortTag Sell = Invested


extend :: Int -> Vector Int -> Vector Int
extend len vs =
  let h = Vec.head vs
      vs0 = if h > 0 then Vec.cons h vs else vs
      l = Vec.last vs
      vs1 = if l < len then Vec.snoc vs len else vs
  in vs1

impulse2tradeListHelp ::
  (Ord t) => (Impulse -> Position) -> Signal t ohlc -> ImpulseSignal t -> TradeList stgy t ohlc
impulse2tradeListHelp tag (Signal ps) (ImpulseSignal is) =
  let idxs = extend (Vec.length ps - 1) (Vec.findIndices ((`Set.member` (Map.keysSet is)) . fst) ps)
      f i j = Vec.slice i (j-i+1) ps
      xs = Vec.zipWith f idxs (stail "stail: impulse2tradeListHelp" idxs)
      g zs =
        let (t, _) = shead "shead: impulse2tradeListHelp" zs
        in maybe (error "impulse2tradeListHelp Nothing") (\sb -> Trade (tag sb) zs) (Map.lookup t is)
  in TradeList (Vec.toList (Vec.map g xs))


instance Impulse2TradeList Long where
  impulse2tradeList = impulse2tradeListHelp longTag

instance Impulse2TradeList Short where
  impulse2tradeList = impulse2tradeListHelp shortTag 
