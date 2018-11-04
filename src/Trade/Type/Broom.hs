

module Trade.Type.Broom where

import qualified Prelude as P
import Prelude hiding (zip, zipWith)

import Trade.Type.Equity (Equity)
import Trade.Type.Signal (Signal)
import Trade.Type.StepFunc (StepFunc)

import qualified Trade.Type.Conversion.Equity2Yield as E2Y
import qualified Trade.Type.Conversion.Yield2Equity as Y2E

-- import Trade.Report.Curve -- (Curve, curve)
import qualified Trade.Report.Line as Line

-- | A broom is a list of histories that we can analyse for profit and risk.
newtype Broom signal = Broom {
  unBroom :: [signal]
  } deriving (Show, Eq)


instance Functor Broom where
  fmap f (Broom hs) = Broom (map f hs)


zipWith :: (a -> b -> c) -> Broom a -> Broom b -> Broom c
zipWith f (Broom xs) (Broom ys) = Broom (P.zipWith f xs ys)

zip :: Broom a -> Broom b -> Broom (a, b)
zip = zipWith (,)

-- | Turn a broom into a chart with `n` curves.
broom2chart :: (Line.Line signal) => Int -> Broom signal -> [Line.L [(Line.TyX signal, Line.TyY signal)]]
broom2chart n (Broom xs) =
  let f i x = Line.line (show i) x
  in P.zipWith f [0 :: Integer ..] (take n xs)



-- | TODO: Testing (yield2equity . equity2yield) == id
yield2equity ::
  (Y2E.Yield2Equity yield) =>
  StepFunc yield -> Equity -> Broom (Signal t yield) -> Broom (Signal t Equity)
yield2equity step eqty = fmap (Y2E.yield2equity step eqty)



equity2yield :: (E2Y.Equity2Yield yield) => Broom (Signal t Equity) -> Broom (Signal t yield)
equity2yield = fmap E2Y.equity2yield
