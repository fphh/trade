{-# LANGUAGE TypeFamilies #-}


module Trade.Type.Distribution where

import Data.Vector (Vector)

import qualified Trade.Report.Line as L

type Percent = Double


newtype CDF a = CDF {
  unCDF :: Vector (Percent, Double)
  } deriving (Show)

{-
instance L.Line (CDF a) where
  type TyX (CDF a) = Percent
  type TyY (CDF a) = Double
  line str (CDF vs) = L.line str vs
-}
