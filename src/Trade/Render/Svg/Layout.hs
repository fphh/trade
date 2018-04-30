

module Trade.Svg.Layout where


newtype Height = Height Double deriving  (Show)
newtype InnerHeight = InnerHeight Double deriving  (Show)

newtype Width = Width Double deriving  (Show)
newtype InnerWidth = InnerWidth Double deriving  (Show)

newtype XMargin = XMargin Double deriving (Show)

newtype YMargin = YMargin Double deriving (Show)

newtype TickLength = TickLength Double deriving (Show)

data Layout = Layout {
  width :: Width
  , height :: Height
  , xMargin :: XMargin
  , yMargin :: YMargin
  , tickLength :: TickLength
  } deriving (Show)

innerWidth :: Layout -> InnerWidth
innerWidth layout =
  let Width w = width layout
      XMargin xm = xMargin layout
  in InnerWidth (w - 2*xm)

innerHeight :: Layout -> InnerHeight
innerHeight layout =
  let Height h = height layout
      YMargin ym = yMargin layout
  in InnerHeight (h - 2*ym)

defaultLayout :: Layout
defaultLayout = Layout {
  width = Width 1200
  , height = Height 600
  , xMargin = XMargin 120
  , yMargin = YMargin 80
  , tickLength = TickLength 5
  }

