{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}


module Trade.Type.Bars where

import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime)


-- import qualified Graphics.Rendering.Chart.Easy as E


type family DeltaTy t :: *

type instance DeltaTy UTCTime = NominalDiffTime

class Add t where
  add :: DeltaTy t -> t -> t
  diff :: t -> t -> DeltaTy t


instance Add UTCTime where
  add dt t = addUTCTime dt t
  diff x y = diffUTCTime x y



newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord) -- , E.PlotValue)

newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord)

type instance DeltaTy BarNo = Bars


instance Add BarNo where
  add (Bars dt) (BarNo t) = BarNo (dt+t)
  diff (BarNo x) (BarNo y) = Bars (x-y)



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

