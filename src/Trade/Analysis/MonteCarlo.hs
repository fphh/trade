

module Trade.Analysis.MonteCarlo where

import qualified Data.Vector as Vec

import qualified Data.Map.Strict as Map

import System.Random

import Trade.Type.Bars (Bars(..))

import Trade.Trade.TradeList

import Trade.Analysis.Yield
import Trade.Analysis.OffsettedNormTradeList


startingOffsets :: NormTradeList ohlc -> (Int -> Bars)
startingOffsets (NormTradeList tl) =
  let f (NormTrade _ _ ys) = Vec.imap (\i _ -> Bars i) ys
        -- Vec.generate (Vec.length ys) (Bars . id)
      table = Vec.concat (map f tl)
      len = Vec.length table
  in \n -> table Vec.! (n `mod` len)


randomYieldSignal' :: NormTradeList ohlc -> [Int] -> NormTradeList ohlc
randomYieldSignal' _ [] = error "randomYieldSignal': no random numbers"
randomYieldSignal' ys (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      
      (_, NormTradeList ys0'):(_, NormTradeList ys1'):_ =
        case Map.toList (sortNormTradeByState ys) of
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


randomYieldSignal :: NormTradeList ohlc -> (Int -> Bars) -> IO (OffsettedNormTradeList ohlc)
randomYieldSignal ys offsetTable = do
    gen <- newStdGen
    let i:is = map abs (randoms gen)
        offs = offsetTable i
        rysig = randomYieldSignal' ys is
    return (OffsettedNormTradeList offs rysig)

