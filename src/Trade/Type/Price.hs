{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Price where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Type.Delta (Delta(..), CDelta, DeltaType, DeltaTy, Add, add, diff)
import Trade.Type.Scale (Scale, scale, factor)

import Trade.Report.Pretty (Pretty, pretty)


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, Num, E.PlotValue)


instance Pretty Price where
  pretty = show

instance Type2Double Price where
  type2double (Price x) = x

instance DeltaType Price where
  type DeltaTy Price = CDelta Price

  
instance Add Price where
  add (Delta dp) (Price p) = Price (p+dp)
  diff (Price p) (Price q) = Delta (p-q)


instance Scale Price where
  factor q (Price p) = q/p
  scale q (Price p) = Price (q*p)


{-
instance Scale (Delta Price) where
  factor q (Delta dp) = q/dp
  scale q (Delta dp) = Delta (q*dp)
-}
