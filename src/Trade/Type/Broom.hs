

module Trade.Type.Broom where

import Trade.Type.Yield (Yield)
import Trade.Type.Equity (Equity)
import Trade.Type.History (History)
import Trade.Type.StepFunc (StepFunc)
import qualified Trade.Type.History as Hist


-- | A broom is a list of histories that we can analyse for profit and risk.
newtype Broom history = Broom {
  unBroom :: [history]
  } deriving (Show, Eq)


instance Functor Broom where
  fmap f (Broom hs) = Broom (map f hs)

yield2equity ::
  StepFunc -> Equity -> Broom (History Yield) -> Broom (History Equity)
yield2equity step eqty = fmap (Hist.yield2equity step eqty)


equity2yield :: Broom (History Equity) -> Broom (History Yield)
equity2yield = fmap Hist.equity2yield

