{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Equity where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)


import Prelude hiding ((+), (*), (/))
import qualified Prelude as Prelude

import Trade.Report.Pretty

-- | Equity that you own at some point in time.
newtype Equity = Equity {
  unEquity :: Double
  } deriving (Show, Read, Eq, Ord, E.PlotValue)

instance Pretty Equity where
  pretty (Equity x) = "Equity=" ++ show x

instance Type2Double Equity where
  type2double = unEquity
