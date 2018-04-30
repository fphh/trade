

module Trade.Render.Svg.AxisTicks.Real where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Ratio

import Text.Printf

data Magnitude =
  Pico
  | Nano
  | Micro
  | Milli
  | One
  | Kilo
  | Mega
  | Giga
  | Tera
  deriving (Show, Enum, Eq, Ord)


steps :: [(Ratio Integer, Magnitude)]
steps = zip (iterate (*1000) pico) [Pico .. Tera]

pico :: Ratio Integer
pico = 1 % (10^12)

stepTicks :: [(Ratio Integer, Ratio Integer)]
stepTicks =
  let as = map (*pico) [ 1 % 10, 1 % 8, 15 % 100, 2 % 10, 1 % 4, 1 % 2 ] ++ map (*10) as
  in zip as (tail as)


startTicks :: [(Ratio Integer, Ratio Integer)]
startTicks =
  let as = map (*pico) [0, 100 .. 900] ++ map (*1000) as
  
  in zip as (tail as)

findNearest :: [(Ratio Integer, Ratio Integer)] -> Ratio Integer -> Ratio Integer
findNearest ts t =
  case filter (\(x, y) -> x < t && t <= y) ts of
    (a, b):_ -> case t-a < b-t of
                 True -> a
                 False -> b

prevMags :: Map Magnitude Magnitude
prevMags = Map.fromList $ zip (tail [Pico .. Tera]) [Pico .. Tera]

magBefore :: Magnitude -> Magnitude
magBefore m = maybe Pico id (Map.lookup m prevMags)

magnitude :: Ratio Integer -> Magnitude
magnitude x =
  case dropWhile ((<=x) . fst) steps of
    [] -> Tera
    (_, m):_ -> magBefore m

vsteps :: Map Magnitude (Ratio Integer)
vsteps = Map.fromList (map (\(x,y) -> (y,x)) steps)

factor :: Magnitude -> Ratio Integer
factor m = vsteps Map.! m


intervalAndStep :: Double -> Double -> (Magnitude, Ratio Integer)
intervalAndStep a b =
  let d = approxRational (b - a) (10**(-12))
      dd = d / 10
      m = magnitude dd
      t = findNearest stepTicks dd
  in (m, t)

format :: Double -> String
format x = flip printf x $
  case x of
    y | y == 0 -> "%f"
    y | y < 0.001 -> "%e"
    y | y < 1 -> "%.4f"
    y | y < 10 -> "%1.3f"
    y | y < 100 -> "%2.2f"
    y | y < 1000 -> "%3.2f"
    y | y < 10000 -> "%4.1f"
    y -> "%e"

ticksReal :: Double -> Double -> (Double -> String, [Double])
ticksReal a b =
  let (i, s) = intervalAndStep a b
      a' = approxRational a (10**(-12))
      firstTick = findNearest startTicks (abs (a'-(s/2)))
      ts = iterate (+s) firstTick
      cut = takeWhile (<=b) . dropWhile (< a)
      
  in (format, cut $ map realToFrac ts)
  
