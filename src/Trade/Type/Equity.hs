{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Trade.Type.Equity where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Pretty (Pretty, pretty, formatf)


newtype Equity = Equity {
  unEquity :: Double
  } deriving (Show, Read, Eq, Ord, Num, Fractional, Floating, E.PlotValue)


instance Pretty Equity where
  pretty = formatf unEquity "Eqt"
