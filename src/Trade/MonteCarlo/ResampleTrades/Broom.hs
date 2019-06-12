{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.MonteCarlo.ResampleTrades.Broom where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

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

import Trade.Type.Broom (Broom(..))

import qualified Trade.Type.Signal as Signal
import Trade.Type.Signal (Timeseries, Signal)


{-

randomDelta ::
  Vector (DeltaSignal ohlc) -> Vector (DeltaSignal ohlc) -> [Int] -> [DeltaSignal ohlc]
randomDelta _ _ [] = error "Trade.MonteCarlo.ResampleTrades.Broom.randomDelta: empty list"
randomDelta is nis (r:rs) =
  let as =
        case odd r of
          False -> let xs = nis:is:xs in xs
          True -> let xs = is:nis:xs in xs
        
      f i a = a Vec.! (i `mod` Vec.length a) 

  in zipWith f rs as


deltaBroom :: Experiment.Output stgy sym ohlc -> Broom (IO [DeltaSignal ohlc])
deltaBroom (Experiment.Output _ _ ds _ _ _) =
  let (_, DeltaTradeList dtl):_ = Map.toList ds
      f x@(DeltaSignal _ pos _) (us, vs) =
        case pos of
          Invested -> (x:us, vs)
          NotInvested -> (us, x:vs)

      toVec (js, ks) = (Vec.fromList js, Vec.fromList ks)
      (is, nis) = toVec (List.foldr f ([], []) dtl)

      g = newStdGen >>= return . randomDelta is nis . randoms
  in Broom (repeat g)

-}

newtype MCCount = MCCount {
  unMCCount :: Int
  } deriving (Show)


{-

boundaries ::
  UTCTime -> NominalDiffTime -> MCCount -> Broom (IO [DeltaSignal ohlc]) -> IO (Broom (DeltaTradeList ohlc))
boundaries begin duration (MCCount n) (Broom ioDs) = do
  let maxBarNo = duration `addUTCTime` begin

      ds = take n ioDs

      f _ [] = []
      f pt (x@(DeltaSignal _ _ sig) : xs) =
        let (dt, _) = Signal.last sig
            newPt = dt `addUTCTime` pt
        in case newPt > maxBarNo of
             True -> []
             False -> (x { start = pt }) : f newPt xs

  fmap (Broom . map (DeltaTradeList . f begin)) (sequence ds)
-}



data MCConfig = MCConfig {
  mcBegin :: UTCTime
  , mcBars :: NominalDiffTime
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
  (StepFunction (StepTy stgy)) =>
  Experiment.Result stgy sym ohlc -> MCConfig -> IO (Broom (Timeseries Equity))
mc = undefined

{-  
mc result mtc = do
  let db = deltaBroom (Experiment.output result)
      stp = Experiment.step (Experiment.input result)
      eqty = Experiment.initialEquity (Experiment.input result)
  broom <- boundaries (mcBegin mtc) (mcBars mtc) (mcCount mtc) db
  return (fmap (concatDeltaSignals stp eqty) broom)

-}
