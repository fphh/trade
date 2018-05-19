

module Trade.Trade.ImpulseSignal where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Report.NumberedList
import Trade.Report.Pretty

import Trade.Render.Svg.Plot


data Impulse = Buy | Sell deriving (Show, Eq)

instance Pretty Impulse where
  pretty = show

{-
data ImpulseParameter evt = ImpulseParameter {
  eventToImpulse :: Int -> UTCTime -> evt -> Maybe Impulse
  , events :: Vector (UTCTime, evt)
  }
-}

newtype ImpulseSignal ohcl = ImpulseSignal {
  unImpulseSignal :: Vector (UTCTime, Maybe Impulse)
  } deriving (Show)

instance ToNumberedList (ImpulseSignal ohcl) where
  toNumberedList (ImpulseSignal imps) = toNumberedList imps

toImpulseSignal ::
  (Int -> UTCTime -> evt -> Maybe Impulse)
  -> Vector (UTCTime, evt)
  -> ImpulseSignal ohcl
toImpulseSignal f vs = ImpulseSignal (Vec.imap (\i (t, x) -> (t, f i t x)) vs)

bhImpulse :: Int -> (Int -> UTCTime -> evt -> Maybe Impulse)
bhImpulse len i _ _ | i == 0 = Just Buy
bhImpulse len i _ _ | i == len-1 = Just Sell
bhImpulse _ _ _ _ = Nothing
 
bhImpulseSignal :: Vector (UTCTime, evt) -> ImpulseSignal ohcl
bhImpulseSignal vs = toImpulseSignal (bhImpulse (Vec.length vs - 1)) vs

--bhImpulseParameter :: Vector (UTCTime, ohcl) -> ImpulseParameter ohcl
--bhImpulseParameter vs = ImpulseParameter (bhImpulse (Vec.length vs - 1)) vs



data ImpulseArgs = ImpulseArgs {
  middle :: Double
  , spikeLength :: Double
  } deriving (Show)


impulse2line' :: ImpulseArgs -> ImpulseSignal ohlc -> Vector (UTCTime, Double)
impulse2line' (ImpulseArgs m len) (ImpulseSignal imps) =
  let l = m-len
      h = m+len
      f (t, Just Sell) = [(t, m), (t, h), (t, m)]
      f (t, Just Buy) = [(t, m), (t, l), (t, m)]
      f _ = []
  in Vec.fromList (concatMap f (Vec.toList imps))

  
impulse2line :: ImpulseArgs -> ImpulseSignal ohlc -> PlotItem Vector UTCTime
impulse2line args imps = Line "impulses" (impulse2line' args imps)
