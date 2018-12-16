{-# LANGUAGE TypeFamilies #-}


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
import Trade.Type.Yield (NoYield)

import Trade.Type.Conversion.OffsettedNormTradeList2NormSignal (offsettedNormTradeList2normSignal)
import Trade.Type.Conversion.NormTrade2YieldSignal (yieldAccordingToPosition)

import Trade.Analysis.Yield (sortNormTradesByPosition)


startingOffsets :: NormTradeList yield t -> (Int -> Bars)
startingOffsets (NormTradeList tl) =
  let f (NormTrade _ _ ys) = Vec.imap (\i _ -> Bars i) ys
      table = Vec.concat (map f tl)
      len = Vec.length table
  in \n -> table Vec.! (n `mod` len)


randomYieldSignal' :: NormTradeList yield t -> [Int] -> NormTradeList yield t
randomYieldSignal' _ [] = error "randomYieldSignal': no random numbers"
randomYieldSignal' ys (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      
      (_, NormTradeList ys0'):(_, NormTradeList ys1'):_ =
        case Map.toList (sortNormTradesByPosition ys) of
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


randomYieldSignal :: NormTradeList yield t -> (Int -> Bars) -> IO (OffsettedNormTradeList yield t)
randomYieldSignal ys offsetTable = do
    gen <- newStdGen
    let i:is = map abs (randoms gen)
        offs = offsetTable i
        rysig = randomYieldSignal' ys is
    return (OffsettedNormTradeList offs rysig)


normBroom :: (NoYield yield) => Bars -> Int -> NormTradeList yield t -> IO (Broom (Signal BarNo yield))
normBroom bs n ntl = do
  let soffs = startingOffsets ntl
      ntl' = NormTradeList (map yieldAccordingToPosition (unNormTradeList ntl))
  offsTls <- replicateM n (randomYieldSignal ntl' soffs)
  return (Broom (map (offsettedNormTradeList2normSignal bs) offsTls))

