{-# LANGUAGE TypeFamilies #-}


module Trade.Timeseries.Timeseries where

import Data.Time.Clock (UTCTime)

import Trade.Type.EquityAndShare (Open(..), Close(..), Low(..), High(..), Volume(..))

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing)

import Trade.Timeseries.Quandl.Row


timeseriesFromMaybe :: Vector (UTCTime, Maybe a) -> (Double, Vector (UTCTime, a))
timeseriesFromMaybe vs =
  let (nothings, bs) = Vec.partition isNothing (Vec.map sequence vs)
      p Nothing = False
      ratio = fromIntegral (Vec.length nothings) / fromIntegral (Vec.length vs)
      newVs = case Vec.sequence bs of
                Just xs -> xs
                Nothing -> error "Trade.Timeseries.Timeseries.fromMaybe: you should never be here!"
  in (ratio, newVs)


toTimeseries :: (a -> b) -> Vector (UTCTime, Maybe a) -> Vector (UTCTime, b)
toTimeseries f vs =
  let (_, vs') = timeseriesFromMaybe vs
  in fmap (fmap f) vs'

extractMaybeFromRow ::
  (DateInterface row) =>
  (row -> Maybe a) -> Vector row -> Vector (UTCTime, Maybe a)
extractMaybeFromRow f = Vec.map (\r -> (dateDI r, f r))

extractFromRow ::
  (DateInterface row) =>
  (row -> Maybe a) -> Vector row -> Vector (UTCTime, a)
extractFromRow f vs = snd (timeseriesFromMaybe (extractMaybeFromRow f vs))


class Timeseries a where
  type TSTy a :: *
    
  timeseries :: Vector (UTCTime, Maybe a) -> Vector (UTCTime, TSTy a)

  timeseriesBy ::
    (DateInterface row) =>
    (row -> Maybe a) -> Vector row -> Vector (UTCTime, TSTy a)
  timeseriesBy f = timeseries . extractMaybeFromRow f


instance Timeseries Open where
  type TSTy Open = Double
  timeseries = toTimeseries unOpen

instance Timeseries Close where
  type TSTy Close = Double
  timeseries = toTimeseries unClose

instance Timeseries Low where
  type TSTy Low = Double
  timeseries = toTimeseries unLow

instance Timeseries High where
  type TSTy High = Double
  timeseries = toTimeseries unHigh

instance Timeseries Volume where
  type TSTy Volume = Int
  timeseries = toTimeseries unVolume
