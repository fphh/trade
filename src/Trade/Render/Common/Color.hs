

module Trade.Render.Common.Color where



data Color =
  Red | Blue | Green | Magenta | Orange | Darkcyan | Black | Gray | Purple | Pink deriving (Show, Enum, Bounded)

colors :: [Color]
colors = cycle [minBound .. maxBound]

