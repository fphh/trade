
module Trade.MonteCarlo.ResampleTrades.Broom where

import System.Random (newStdGen, randoms)

import qualified Data.Map as Map

import qualified Data.Vector as Vec

import Control.Monad (replicateM)

import Trade.Type.Bars (Bars(..), BarNo)
import Trade.Type.Broom (Broom(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))
import Trade.Type.OffsettedNormTradeList (OffsettedNormTradeList(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.State (State(..))
import Trade.Type.Yield (Yield(..))

import Trade.Type.Conversion.OffsettedNormTradeList2NormSignal (offsettedNormTradeList2normSignal)

import Trade.Analysis.Yield (sortNormTradesByState)


startingOffsets :: NormTradeList -> (Int -> Bars)
startingOffsets (NormTradeList tl) =
  let f (NormTrade _ _ ys) = Vec.imap (\i _ -> Bars i) ys
      table = Vec.concat (map f tl)
      len = Vec.length table
  in \n -> table Vec.! (n `mod` len)


randomYieldSignal' :: NormTradeList -> [Int] -> NormTradeList
randomYieldSignal' _ [] = error "randomYieldSignal': no random numbers"
randomYieldSignal' ys (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      
      (_, NormTradeList ys0'):(_, NormTradeList ys1'):_ =
        case Map.toList (sortNormTradesByState ys) of
          cs@(_:_:_) -> cs
          _ -> error "randomYieldSignal': not enough types of trades available"
      
      ys0 = Vec.fromList ys0'
      ys0Len =
        case Vec.length ys0 of
          0 -> error "randomYieldSignal': y0 has zero length"
          n -> n

      ys1 = Vec.fromList ys1'
      ys1Len =
        case Vec.length ys1 of
          0 -> error "randomYieldSignal': y1 has zero length"
          n -> n

      f n k =
        case n of
          0 -> ys0 Vec.! (k `mod` ys0Len)
          1 -> ys1 Vec.! (k `mod` ys1Len)
          _ -> error "randomYieldSignal': never here"

      trades = zipWith f tradeTypes is
      
  in NormTradeList trades


randomYieldSignal :: NormTradeList -> (Int -> Bars) -> IO OffsettedNormTradeList
randomYieldSignal ys offsetTable = do
    gen <- newStdGen
    let i:is = map abs (randoms gen)
        offs = offsetTable i
        rysig = randomYieldSignal' ys is
    return (OffsettedNormTradeList offs rysig)


normBroom :: Bars -> Int -> NormTradeList -> IO (Broom (Signal BarNo Yield))
normBroom bs n ntl = do
  let soffs = startingOffsets ntl
      f (NormTrade NoPosition t vs) =
        NormTrade NoPosition t (Vec.replicate (Vec.length vs + 1) (Yield 1))
      f (NormTrade state t vs) =
        NormTrade state t (Vec.cons (Yield 1) vs)

      ntl' = NormTradeList (map f (unNormTradeList ntl))
      
  offsTls <- replicateM n (randomYieldSignal ntl' soffs)

  return (Broom (map (offsettedNormTradeList2normSignal bs) offsTls))

