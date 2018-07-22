

module Trade.Type.Broom where

import Trade.Type.Yield (Yield)
import Trade.Type.Equity (Equity)
import Trade.Type.History (History)
import Trade.Type.StepFunc (StepFunc)
import qualified Trade.Type.History as Hist

import Trade.Report.Curve -- (Curve, curve)
import qualified Trade.Report.Report as Report

-- | A broom is a list of histories that we can analyse for profit and risk.
newtype Broom history = Broom {
  unBroom :: [history]
  } deriving (Show, Eq)


instance Functor Broom where
  fmap f (Broom hs) = Broom (map f hs)


-- | Turn a broom into a chart with `n` curves.
broom2chart :: (Curve history) => Int -> Broom history -> [Report.LineTy (Ty history) Double]
broom2chart n (Broom xs) =
  let f i x = Report.line (show i) (curve x)
  in zipWith f [0 :: Integer ..] (take n xs)


-- | TODO: Testing (yield2equity . equity2yield) == id
yield2equity ::
  StepFunc -> Equity -> Broom (History Yield) -> Broom (History Equity)
yield2equity step eqty = fmap (Hist.yield2equity step eqty)


equity2yield :: Broom (History Equity) -> Broom (History Yield)
equity2yield = fmap Hist.equity2yield
