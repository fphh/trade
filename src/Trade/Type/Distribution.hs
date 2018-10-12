{-# LANGUAGE TypeFamilies #-}


module Trade.Type.Distribution where

import Data.Vector (Vector)

import qualified Trade.Report.Report as Report

type Percent = Double

newtype CDF a = CDF {
  unCDF :: Vector (Percent, Double)
  } deriving (Show)


instance Report.Line (CDF a) where
  type TyX (CDF a) = Percent
  type TyY (CDF a) = Double
  line str (CDF vs) = Report.line str vs
