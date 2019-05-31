{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.BarLength where

import Data.Time.Clock (NominalDiffTime)


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
