{-# LANGUAGE TypeFamilies #-}


module Trade.Timeseries.Timeseries where

import Data.Time.Clock (UTCTime)

import Trade.Type.EquityAndShare (Open(..), Close(..), Low(..), High(..), Volume(..))

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing)

import Trade.Timeseries.Row



extractMaybeFromRow ::
  (DateInterface row) =>
  (row -> Maybe a) -> Vector row -> Vector (UTCTime, Maybe a)
extractMaybeFromRow f = Vec.map (\r -> (dateDI r, f r))


timeseriesFromMaybe :: Vector (UTCTime, Maybe a) -> (Double, Vector (UTCTime, a))
timeseriesFromMaybe vs =
  let (nothings, bs) = Vec.partition isNothing (Vec.map sequence vs)
      ratio = fromIntegral (Vec.length nothings) / fromIntegral (Vec.length vs)
      newVs = case Vec.sequence bs of
                Just xs -> xs
                Nothing -> error "Trade.Timeseries.Timeseries.fromMaybe: you should never be here!"
  in (ratio, newVs)

extractFromRow ::
  (DateInterface row) =>
  (row -> Maybe a) -> Vector row -> Vector (UTCTime, a)
extractFromRow f vs = snd (timeseriesFromMaybe (extractMaybeFromRow f vs))

class Timeseries a where
  type TSTy a :: *
    
  timeseries :: Vector (UTCTime, a) -> Vector (UTCTime, TSTy a)

instance Timeseries Open where
  type TSTy Open = Double
  timeseries = Vec.map (fmap unOpen)

instance Timeseries Close where
  type TSTy Close = Double
  timeseries = Vec.map (fmap unClose)


instance Timeseries Low where
  type TSTy Low = Double
  timeseries = Vec.map (fmap unLow)

instance Timeseries High where
  type TSTy High = Double
  timeseries = Vec.map (fmap unHigh)

instance Timeseries Volume where
  type TSTy Volume = Integer
  timeseries = Vec.map (fmap unVolume)


timeseriesBy ::
  (DateInterface row, Timeseries a) =>
  (row -> Maybe a) -> Vector row -> Vector (UTCTime, TSTy a)
timeseriesBy f = timeseries . extractFromRow f

