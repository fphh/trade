{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Analysis.MonteCarlo where

import Control.Monad.State.Lazy

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time.Clock -- (UTCTime, NominalDiffTime)

import System.Random

import Trade.Trade.Signal -- (YieldSignal(..))
import Trade.Analysis.Yield

import Debug.Trace


startingOffsets :: YieldSignal ohlc -> Map NominalDiffTime Int
startingOffsets (YieldSignal ys) =
  let day = 24*60*60
      f acc (TradeYield _ dur _) =
        let days = map (day*) [0, 1 .. (dur / day)]
            g m d = Map.insertWith' (+) d 1 m
        in List.foldl' g acc days
  in Vec.foldl' f Map.empty (Vec.map snd ys)


randomOffset :: Map NominalDiffTime Int -> Int -> NominalDiffTime
randomOffset m n =
  let s = sum (Map.elems m)
      xs = Map.toList m
      f (x, acc) (dt, cnt) = (x+cnt, (x, x+cnt-1, dt):acc)
      table :: [(Int, Int, NominalDiffTime)]
      table = snd (List.foldl' f (0, []) xs)
      n' = n `mod` s
  in case dropWhile (\(p, q, _) -> n' < p) table of
       [] -> error "randomStartingPoint: not possible"
       (_, _, startingDiffTime):_ -> startingDiffTime

randomYieldSignal' :: forall ohlc. UTCTime -> UTCTime -> YieldSignal ohlc -> NominalDiffTime -> [Int] -> YieldSignal ohlc
randomYieldSignal' begin end ys offset (i:is) =
  let j = i `mod` 2
      tradeTypes = j:(1 - j):tradeTypes
      (_, YieldSignal ys0'):(_, YieldSignal ys1'):_ = Map.toList (sortYieldByState ys)
      
      ys0 = Vec.map snd ys0'
      ys0Len = Vec.length ys0

      ys1 = Vec.map snd ys1'
      ys1Len = Vec.length ys1


      f 0 i = ys0 Vec.! (i `mod` ys0Len)
      f 1 i = ys1 Vec.! (i `mod` ys1Len)
      f _ _ = error "randomYieldSignal': never here"

      trades :: [TradeYield ohlc]
      trades = zipWith f tradeTypes is
      
      start = offset `addUTCTime` begin

      g t tr = yieldDuration tr `addUTCTime` t
      times = scanl g start trades
  in YieldSignal (Vec.fromList (takeWhile ((<end) . fst) (zip times trades)))

randomYieldSignal :: UTCTime -> UTCTime -> YieldSignal ohlc -> IO (YieldSignal ohlc)
randomYieldSignal begin end ys = do
    gen <- newStdGen
    let i:is = randoms gen
        offset = randomOffset (startingOffsets ys) i
    return (randomYieldSignal' begin end ys offset is) 
