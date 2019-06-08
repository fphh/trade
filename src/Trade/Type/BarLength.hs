{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.BarLength where

import Data.Time.Clock (NominalDiffTime)

import qualified Data.Text.Lazy as Text

import Formatting ((%), format, int)
import Data.String (fromString)


import Trade.Report.Pretty (Pretty, pretty)

data BarLength =
  Sec Int
  | Min Int
  | Hour Int
  | Day Int
  | Week Int
  | Month Int
  deriving (Eq, Show)

barLength2diffTime :: BarLength -> NominalDiffTime
barLength2diffTime t = fromIntegral $
  case t of
    Sec x -> x
    Min x -> 60*x
    Hour x -> 60*60*x
    Day x -> 24*60*60*x
    Week x -> 7*24*60*60*x
    Month x -> 30*24*60*60*x

instance Pretty BarLength where
  pretty bl = pretty $
    case bl of
      Sec x -> realToFrac x :: NominalDiffTime
      Min x -> realToFrac (60*x)
      Hour x -> realToFrac (60*60*x)
      Day x -> realToFrac (24*60*60*x)
      Week x -> realToFrac (7*24*60*60*x)
      Month x -> realToFrac (30*24*60*60*x)


newtype Bars = Bars {
  unBars :: Int
  } deriving (Eq, Show)

instance Pretty Bars where
  pretty (Bars b) = Text.unpack (format (int % fromString " bars") b)
