

module Trade.Trade.StateSignal where


import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (catMaybes, isJust)

import Trade.Report.NumberedList
import Trade.Report.Pretty

import Trade.Trade.ImpulseSignal
import Trade.Trade.State

import Trade.Help.SafeTail

data StateInterval = StateInterval {
  duration :: NominalDiffTime
  , state :: State
  } deriving (Show)

instance Pretty StateInterval where
  pretty (StateInterval d s) = pretty d ++ ", " ++ pretty s

data StateSignal ohlc = StateSignal {
  start :: UTCTime
  , signal :: Vector (UTCTime, StateInterval)
  } deriving (Show)

instance ToNumberedList (StateSignal ohlc) where
  toNumberedList (StateSignal strt ss) = [show strt] : toNumberedList ss


impulse2state :: ImpulseSignal ohlc -> StateSignal ohlc
impulse2state (ImpulseSignal is) =
  let (s, _) = Vec.head is
  
      js = Vec.fromList
           $ catMaybes
           $ Vec.toList
           $ case Vec.filter isJust (Vec.map sequence is) of
               xs | Vec.null xs -> error "impulse2state: no impulses in sequence"
               xs -> xs

      f (t0, Sell) (t1, Buy)  = (t0, StateInterval (t1 `diffUTCTime` t0) NoPosition)
      f (t0, Buy) (t1, Sell)  = (t0, StateInterval (t1 `diffUTCTime` t0) Long)
      f _ _ = error "impulse2state: unexpected sequence of impulses"

  in StateSignal s (Vec.zipWith f js (stail "impulse2state" js))
