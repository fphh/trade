

module Trade.Render.Svg.AxisTicks where

import Data.Time.Clock (UTCTime)

import Trade.Render.Svg.AxisTicks.Time (ticksUTC)
import Trade.Render.Svg.AxisTicks.Real (ticksReal)


class AxisTicks a where
  axisTicks :: a -> a -> (a -> String, [a])

instance AxisTicks UTCTime where
  axisTicks = ticksUTC

instance AxisTicks Double where
  axisTicks = ticksReal
