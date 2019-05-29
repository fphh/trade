{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.MonteCarlo.ResampleTrades.Broom where

import System.Random (newStdGen, randoms)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map

import qualified Data.List as List

import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)

import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Position (Position(..))

import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)

import qualified Trade.Type.Experiment as Experiment

import Trade.Type.Bars (DeltaTy, Add, add)
import Trade.Type.Broom (Broom(..))

import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal (Signal)



randomDelta ::
  Vector (DeltaSignal t ohlc) -> Vector (DeltaSignal t ohlc) -> [Int] -> [DeltaSignal t ohlc]
randomDelta _ _ [] = error "Trade.MonteCarlo.ResampleTrades.Broom.randomDelta: empty list"
randomDelta is nis (r:rs) =
  let as =
        case odd r of
          False -> let xs = nis:is:xs in xs
          True -> let xs = is:nis:xs in xs
        
      f i a = a Vec.! (i `mod` Vec.length a) 

  in zipWith f rs as


deltaBroom :: Experiment.Output stgy sym t ohlc -> Broom (IO [DeltaSignal t ohlc])
deltaBroom (Experiment.Output _ _ ds _) =
  let (_, DeltaTradeList dtl):_ = Map.toList ds
      f x@(DeltaSignal _ pos _) (us, vs) =
        case pos of
          Invested -> (x:us, vs)
          NotInvested -> (us, x:vs)

      toVec (js, ks) = (Vec.fromList js, Vec.fromList ks)
      (is, nis) = toVec (List.foldr f ([], []) dtl)

      g = newStdGen >>= return . randomDelta is nis . randoms
  in Broom (repeat g)



newtype MCCount = MCCount {
  unMCCount :: Int
  } deriving (Show)



boundaries ::
  (Ord t, Add t) =>
  t -> DeltaTy t -> MCCount -> Broom (IO [DeltaSignal t ohlc]) -> IO (Broom (DeltaTradeList t ohlc))
boundaries begin duration (MCCount n) (Broom ioDs) = do
  let maxBarNo = duration `add` begin

      ds = take n ioDs

      f _ [] = []
      f pt (x@(DeltaSignal _ _ sig) : xs) =
        let (dt, _) = Signal.last sig
            newPt = dt `add` pt
        in case newPt > maxBarNo of
             True -> []
             False -> (x { start = pt }) : f newPt xs

  fmap (Broom . map (DeltaTradeList . f begin)) (sequence ds)



data MCConfig t = MCConfig {
  mcBegin :: t
  , mcBars :: DeltaTy t
  , mcCount :: MCCount
  }

{-
defaultMC :: MCConfig BarNo
defaultMC = MCConfig {
  mcBars = Bars 500
  , mcCount = MCCount 1000
  }
-}


mc ::
  (Ord t, Add t, StepFunction (StepTy stgy) t) =>
  Experiment.Result stgy sym t ohlc -> MCConfig t -> IO (Broom (Signal t Equity))
mc result mtc = do
  let db = deltaBroom (Experiment.output result)
      stp = Experiment.step (Experiment.input result)
      eqty = Experiment.initialEquity (Experiment.input result)
  broom <- boundaries (mcBegin mtc) (mcBars mtc) (mcCount mtc) db
  return (fmap (concatDeltaSignals stp eqty) broom)





{-
import System.Random (newStdGen, randoms)

import qualified Data.Map as Map

import qualified Data.Vector as Vec

import Control.Monad (replicateM)

import Trade.Type.Bars (DeltaTy(Bars), BarNo)
import Trade.Type.Broom (Broom(..))
import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.TradeYield (TradeYield(..), TradeYieldList(..))
import Trade.Type.OffsettedTradeYieldList (OffsettedTradeYieldList(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Yield (Yield)

import qualified Trade.Type.Experiment as Experiment

import Trade.Type.Conversion.OffsettedTradeYieldList2NormSignal (offsettedTradeYieldList2normSignal)
import Trade.Type.Conversion.TradeYield2YieldSignal (yieldAccordingToPosition)

import Trade.Analysis.Yield (sortTradeYieldsByPosition)

-}


{-

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

-}



