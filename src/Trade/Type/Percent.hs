{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.Percent where

import qualified Data.Text.Lazy as Text

import Data.String (fromString)

import Formatting (format, (%), fixed)

import Trade.Report.Pretty (Pretty, pretty)

newtype Percent = Percent {
  unPercent :: Double
  } deriving (Show, Eq, Ord, Num, Fractional)



instance Pretty Percent where
  pretty = Text.unpack . format (fixed 2 % fromString "%") . (100*) . unPercent
