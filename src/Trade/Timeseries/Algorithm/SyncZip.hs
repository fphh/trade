{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trade.Timeseries.Algorithm.SyncZip where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Timeseries.Row

syncZipWith ::
  (DateInterface rtx, DateInterface rty, TyD rtx ~ TyD rty, Eq (TyD rty), Ord (TyD rty)) =>
  (TyD rtx -> TyR rtx -> TyR rty -> a) -> Vector rtx -> Vector rty -> (Vector (TyD rtx, a))
syncZipWith f xs ys =
  let as = Vec.toList xs
      bs = Vec.toList ys
      
      go [] _ = []
      go _ [] = []
      go us@(u:us') vs@(v:vs')
        | du == dv = (du, f du (removeDI u) (removeDI v)) : go us' vs'
        | du < dv = go us' vs
        | otherwise {- du > dv -} = go us vs'
        where du = dateDI u
              dv = dateDI v

  in Vec.fromList (go as bs)


class SyncZip rtx rty where
  type TyX rtx :: *
  type TyY rty :: *
  syncZip :: Vector rtx -> Vector rty -> (Vector (TyD rtx, (TyX rtx, TyY rty)))

instance (Ord t) => SyncZip (t, a) (t, b) where
  type TyX (t, a) = a
  type TyY (t, b) = b
  syncZip = syncZipWith (\_ x y -> (x, y))


mapTicker :: (rty -> a) -> Vector rty -> Vector a
mapTicker f vs = Vec.map f vs
