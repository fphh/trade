

module Trade.Common.Color where



data Color =
  OrangeRed | Blue | Green | Magenta | Darkcyan | Black | Gray | Orange | Purple | Pink deriving (Show, Enum, Bounded)

colors :: [Color]
colors = cycle [minBound .. maxBound]

