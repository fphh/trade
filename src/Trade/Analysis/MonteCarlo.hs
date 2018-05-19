

module Trade.Analysis.MonteCarlo where

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

import System.Random

import Trade.Trade.TradeList

import Trade.Analysis.Yield
import Trade.Analysis.OffsettedNormTradeList


startingOffsets :: NormTradeList ohlc -> Map NominalDiffTime Int
startingOffsets (NormTradeList tl) =
  let day = 24*60*60
      f acc (NormTrade _ dur _) =
        let days = map (day*) [0, 1 .. (dur / day)]
            g m d = Map.insertWith (+) d 1 m
        in List.foldl' g acc days
  in List.foldl' f Map.empty tl


randomOffset :: Map NominalDiffTime Int -> Int -> NominalDiffTime
randomOffset m n =
  let s = sum (Map.elems m)
      xs = Map.toList m
      f (x, acc) (dt, cnt) = (x+cnt, (x, x+cnt-1, dt):acc)
      table :: [(Int, Int, NominalDiffTime)]
      table = snd (List.foldl' f (0, []) xs)
      n' = n `mod` s
  in case dropWhile (\(p, _, _) -> n' < p) table of
       [] -> error "randomStartingPoint: not possible"
       (_, _, startingDiffTime):_ -> startingDiffTime


randomYieldSignal' ::
  UTCTime -> UTCTime -> NormTradeList ohlc -> NominalDiffTime -> [Int] -> NormTradeList ohlc
randomYieldSignal' _ _ _ _ [] = error "randomYieldSignal': no random numbers"
randomYieldSignal' begin end ys offs (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      
      (_, NormTradeList ys0'):(_, NormTradeList ys1'):_ =
        case Map.toList (sortNormTradeByState ys) of
          cs@(_:_:_) -> cs
          _ -> error "randomYieldSignal': not enough types of trades available"
      
      ys0 = Vec.fromList ys0'
      ys0Len = Vec.length ys0

      ys1 = Vec.fromList ys1'
      ys1Len = Vec.length ys1


      f 0 k = ys0 Vec.! (k `mod` ys0Len)
      f 1 k = ys1 Vec.! (k `mod` ys1Len)
      f _ _ = error "randomYieldSignal': never here"

      trades = zipWith f tradeTypes is
      
      start = offs `addUTCTime` begin

      g t tr = normTradeDuration tr `addUTCTime` t
      times = scanl g start trades
      
  in NormTradeList (map snd $ takeWhile ((<end) . fst) (zip times trades))


randomYieldSignal :: UTCTime -> UTCTime -> NormTradeList ohlc -> IO (OffsettedNormTradeList ohlc)
randomYieldSignal begin end ys = do
    gen <- newStdGen
    let i:is = randoms gen
        offs = randomOffset (startingOffsets ys) i
    return (OffsettedNormTradeList offs (randomYieldSignal' begin end ys offs is))

