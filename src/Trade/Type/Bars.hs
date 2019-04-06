{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.Bars where

import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime)


import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Pretty (Pretty, pretty)


type family DeltaTy t :: *

type instance DeltaTy UTCTime = NominalDiffTime

class Add t where
  add :: DeltaTy t -> t -> t
  diff :: t -> t -> DeltaTy t


instance Add UTCTime where
  add dt t = addUTCTime dt t
  diff x y = diffUTCTime x y


-- | Duration.
newtype Bars = Bars {
  unBars :: Int
  } deriving (Show, Eq, Ord, Num, Real)

instance Pretty Bars where
  pretty (Bars b) = "b" ++ show b


-- | Point in time.
newtype BarNo = BarNo {
  unBarNo :: Int
  } deriving (Show, Eq, Ord)

type instance DeltaTy BarNo = Bars

instance E.PlotValue BarNo where
  toValue (BarNo x) = E.toValue x
  fromValue = BarNo . E.fromValue

  autoAxis bs =
    let ad = E.autoAxis (map unBarNo bs)
    in E.AxisData {
      E._axis_visibility = E._axis_visibility ad
      , E._axis_viewport = \r (BarNo x) -> E._axis_viewport ad r x
      , E._axis_tropweiv = \r x -> BarNo ( E._axis_tropweiv ad r x)
      , E._axis_ticks = map (\(x, d) -> (BarNo x, d)) (E._axis_ticks ad)
      , E._axis_labels = map (map (\(x, str) -> (BarNo x, "bn" ++ str))) (E._axis_labels ad)
      , E._axis_grid = map BarNo (E._axis_grid ad)
      }


instance Add BarNo where
  add (Bars dt) (BarNo t) = BarNo (dt+t)
  diff (BarNo x) (BarNo y) = Bars (x-y)

instance Pretty BarNo where
  pretty (BarNo b) = "bn" ++ show b


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

