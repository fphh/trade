

module Trade.Analysis.MonteCarlo where

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import System.Random

import Trade.Trade.TradeList

import Trade.Analysis.Yield
import Trade.Analysis.OffsettedNormTradeList
import Trade.Analysis.Bars


startingOffsets :: NormTradeList ohlc -> Map Bars Int
startingOffsets (NormTradeList tl) =
  let f acc (NormTrade _ _ ys) =
        let bs = map Bars [0 .. (Vec.length ys + 1)]
            g m d = Map.insertWith (+) d 1 m
        in List.foldl' g acc bs
  in List.foldl' f Map.empty tl

randomOffset :: Map Bars Int -> Int -> Bars
randomOffset m n =
  let s = sum (Map.elems m)
      xs = Map.toList m
      f (x, acc) (dt, cnt) = (x+cnt, (x, x+cnt-1, dt):acc)
      table = snd (List.foldl' f (0, []) xs)
      n' = n `mod` s
  in case dropWhile (\(p, _, _) -> n' < p) table of
       [] -> error "randomStartingPoint: not possible"
       (_, _, startingDiffTime):_ -> startingDiffTime


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

      f 0 k = ys0 Vec.! (k `mod` ys0Len)
      f 1 k = ys1 Vec.! (k `mod` ys1Len)
      f _ _ = error "randomYieldSignal': never here"

      trades = zipWith f tradeTypes is
      
  in NormTradeList trades


randomYieldSignal :: NormTradeList ohlc -> IO (OffsettedNormTradeList ohlc)
randomYieldSignal ys = do
    gen <- newStdGen
    let i:is = map abs (randoms gen)
        offs = randomOffset (startingOffsets ys) i
        rysig = randomYieldSignal' ys is
    return (OffsettedNormTradeList offs rysig)

