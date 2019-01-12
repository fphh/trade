{-# LANGUAGE TypeFamilies #-}


module Trade.MonteCarlo.ResampleTrades.Broom where

import System.Random (newStdGen, randoms)

import qualified Data.Map as Map

import qualified Data.Vector as Vec

import Control.Monad (replicateM)

import Trade.Type.Bars (DeltaTy(Bars), BarNo)
import Trade.Type.Broom (Broom(..))
import Trade.Type.TradeYield (TradeYield(..), TradeYieldList(..))
import Trade.Type.OffsettedTradeYieldList (OffsettedTradeYieldList(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Yield (Yield)

import Trade.Type.Conversion.OffsettedTradeYieldList2NormSignal (offsettedTradeYieldList2normSignal)
import Trade.Type.Conversion.TradeYield2YieldSignal (yieldAccordingToPosition)

import Trade.Analysis.Yield (sortTradeYieldsByPosition)


startingOffsets :: TradeYieldList -> (Int -> DeltaTy BarNo)
startingOffsets (TradeYieldList tl) =
  let f (TradeYield _ ys) = Vec.imap (\i _ -> Bars i) ys
      table = Vec.concat (map f tl)
      len = Vec.length table
  in \n -> table Vec.! (n `mod` len)


randomYieldSignal' :: TradeYieldList -> [Int] -> TradeYieldList
randomYieldSignal' _ [] = error "randomYieldSignal': no random numbers"
randomYieldSignal' ys (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      
      (_, TradeYieldList ys0'):(_, TradeYieldList ys1'):_ =
        case Map.toList (sortTradeYieldsByPosition ys) of
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
      
  in TradeYieldList trades


randomYieldSignal :: TradeYieldList -> (Int -> DeltaTy BarNo) -> IO (OffsettedTradeYieldList)
randomYieldSignal ys offsetTable = do
    gen <- newStdGen
    let i:is = map abs (randoms gen)
        offs = offsetTable i
        rysig = randomYieldSignal' ys is
    return (OffsettedTradeYieldList offs rysig)


normBroom :: DeltaTy BarNo -> Int -> TradeYieldList -> IO (Broom (Signal BarNo Yield))
normBroom bs n ntl = do
  let soffs = startingOffsets ntl
      ntl' = TradeYieldList (map yieldAccordingToPosition (unTradeYieldList ntl))
  offsTls <- replicateM n (randomYieldSignal ntl' soffs)
  return (Broom (map (offsettedTradeYieldList2normSignal bs) offsTls))

