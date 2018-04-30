
module Trade.Svg.AxisTicks.Time where


import qualified Data.List as List


import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate

import qualified Data.Map as Map
import Data.Map (Map)

data Interval =
  Sec | Min | Hour | Day | Week | Month | Year
  deriving (Show, Eq, Ord, Enum)

intervals :: [Interval]
intervals = [ Sec .. Year ]

prevIntervals :: Map Interval Interval
prevIntervals = Map.fromList $ zip (tail intervals) intervals

prevInterval :: Interval -> Interval
prevInterval x = maybe x id (Map.lookup x prevIntervals)

divI :: Interval -> Integer
divI Sec = 1
divI Min = 60 * divI Sec
divI Hour = 60 * divI Min
divI Day = 24 * divI Hour
divI Week = 7 * divI Day
divI Month = 30 * divI Day
divI Year = 365 * divI Day

factors :: Interval -> Integer
factors x =
  case x of
    Sec -> 1
    Min -> 60
    Hour -> 60
    Day -> 24
    Week -> 7
    Month -> 4
    Year -> 12

ticks :: [(Integer, Interval)]
ticks =
  let is = [Sec .. Year]
  in zip (map divI is) is

interval :: Integer -> Interval
interval x =
  case dropWhile ((x>) . fst) ticks of
    [] -> Year
    (_, t):_ -> prevInterval t
       
findNearest :: Double -> [Double] -> Int
findNearest x xs =
  let f y = (abs (10 - x/y), y)
      (_, r):_ = List.sort $ map f xs
  in round r

roundInterval :: Interval -> Double -> (Interval, Int)
roundInterval i x = (\y -> (i, y)) $ findNearest x $
  case i of
    Sec -> [1, 2, 5, 10, 15, 20, 30]
    Min -> [1, 2, 5, 10, 15, 20, 30]
    Hour -> [1, 2, 3, 6, 12]
    Day -> [1, 2, 3, 7, 14]
    Week -> [1, 2]
    Month -> [1, 2, 3, 6 ]
    Year -> [1, 2, 5, 10, 15, 20, 25, 30, 40, 50, 100]

intervalAndStep :: Integer -> (Interval, Int)
intervalAndStep d =
  let bi = interval d
      pbi = prevInterval bi
      t = fromIntegral d / fromIntegral (divI bi)
      pt = t * (fromIntegral (factors bi))
  in case t of
       x | x <= 6 -> roundInterval pbi pt
       _ -> roundInterval bi t

startOf :: UTCTime -> Int -> Interval -> UTCTime
startOf t _ Sec =
  let q = 10^(12 :: Integer)
      s = (diffTimeToPicoseconds (utctDayTime t) `div` q) * q
  in UTCTime (utctDay t) (picosecondsToDiffTime s)
startOf t _ Min =
  let q = 60*10^(12 :: Integer)
      s = (diffTimeToPicoseconds (utctDayTime t) `div` q) * q
  in UTCTime (utctDay t) (picosecondsToDiffTime s)
startOf t _ Hour =
  let q = 3600*10^(12 :: Integer)
      s = (diffTimeToPicoseconds (utctDayTime t) `div` q) * q
  in UTCTime (utctDay t) (picosecondsToDiffTime s)
startOf t _ Day =
  let (y, m, d) = toGregorian (utctDay t)
  in UTCTime (fromGregorian y m d) 0
startOf t n Week =
  let (y, _, _) = toGregorian (utctDay t)
      (_, d) = toOrdinalDate (utctDay t)
      n' = 7*(n-1)
      q = (d `div` 7) * 7 - n' + 1
  in UTCTime (fromOrdinalDate y q) 0
startOf t _ Month =
  let (y, m, _) = toGregorian (utctDay t)
  in UTCTime (fromGregorian y m 1) 0
startOf t _ Year =
  let (y, _, _) = toGregorian (utctDay t)
  in UTCTime (fromGregorian y 1 1) 0

steps :: UTCTime -> UTCTime -> (Interval, Int)
steps start end = intervalAndStep (round $ nominalDiffTimeToSeconds (end `diffUTCTime` start))


format :: Interval -> Int -> UTCTime -> String
format Sec _ = formatTime defaultTimeLocale "%m-%dT%H:%M:%S"
format Min _ = formatTime defaultTimeLocale "%m-%dT%H:%M"
format Hour _ = formatTime defaultTimeLocale "%m-%d|%H"
format Day _ = formatTime defaultTimeLocale "%y-%m-%d"
format Week _ = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
format Month _ = formatTime defaultTimeLocale "%Y-%m"
format Year _ = formatTime defaultTimeLocale "%Y"

ticksUTC :: UTCTime -> UTCTime -> (UTCTime -> String, [UTCTime])
ticksUTC start end =
  let (i, s) = intervalAndStep (round $ nominalDiffTimeToSeconds (end `diffUTCTime` start))
      s' = fromIntegral s
      firstTick = startOf start s i
      f t =
        let (y, m, _) = toGregorian (utctDay t)
            ms = map (monthLength (isLeapYear y) . (`mod` 12)) [m .. m+s-1]
            ys = map (\x -> if isLeapYear x then 366 else 365) [y .. y+s'-1]
            u = case i of
                  Sec -> s
                  Min -> s * 60
                  Hour -> s * 3600
                  Day -> s * 86400
                  Week -> s * 604800
                  Month -> sum $ map (86400*) ms
                  Year -> sum $ map (86400*) ys
        in addUTCTime (secondsToNominalDiffTime (fromIntegral u)) t

      cut = takeWhile (<=end) . dropWhile (< start)
      
  in (format i s, cut $ iterate f firstTick)

