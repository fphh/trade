{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.Price where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Report.Pretty (Pretty, pretty)


newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, E.PlotValue)


instance Pretty Price where
  pretty = show

instance Type2Double Price where
  type2double (Price x) = x
