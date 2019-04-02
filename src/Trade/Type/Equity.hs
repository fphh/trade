{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.Equity where

import Text.Printf (printf)

import qualified Graphics.Rendering.Chart.Easy as E


import Prelude hiding ((+), (*), (/))
import qualified Prelude as Prelude

import Trade.Report.Pretty


-- | Equity that you own at some point in time.
newtype Equity = Equity {
  unEquity :: Double
  } deriving (Show, Read, Eq, Ord, Num, E.PlotValue)

instance Pretty Equity where
  pretty (Equity x) = printf "%.2fEqty" x
