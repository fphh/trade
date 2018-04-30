{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trade.Quandl.Quandl.Algorithm.SyncZip where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Quandl.Quandl.Row

syncZipWith :: (DateInterface rtx, DateInterface rty) => (rtx -> rty -> a) -> Vector rtx -> Vector rty -> (Vector (UTCTime, a))
syncZipWith f xs ys =
  let as = Vec.toList xs
      bs = Vec.toList ys
      
      go [] _ = []
      go _ [] = []
      go us@(u:us') vs@(v:vs')
        | du == dv = (du, f u v) : go us' vs'
        | du < dv = go us' vs
        | otherwise {- du > dv -} = go us vs'
        where du = dateDI u
              dv = dateDI v

  in Vec.fromList (go as bs)


class SyncZip rtx rty where
  type TyX rtx :: *
  type TyY rty :: *
  syncZip :: Vector rtx -> Vector rty -> (Vector (UTCTime, (TyX rtx, TyY rty)))

instance SyncZip (UTCTime, a) (UTCTime, b) where
  type TyX (UTCTime, a) = a
  type TyY (UTCTime, b) = b
  syncZip = syncZipWith (\x y -> (snd x, snd y))


mapTicker :: (rty -> a) -> Vector rty -> Vector a
mapTicker f vs = Vec.map f vs
