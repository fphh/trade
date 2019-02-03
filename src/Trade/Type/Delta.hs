
module Trade.Type.Delta where

import qualified Prelude as Prelude
import Prelude hiding (negate)

data Delta ty = Delta {
  unDelta :: Double
  } deriving (Show)


negate :: Delta ty -> Delta ty
negate (Delta dx) = Delta (Prelude.negate dx)

id :: Delta ty -> Delta ty
id x = x

zero :: Delta ty
zero = Delta 0.0


class ToDelta p where
  toDelta :: p -> p -> Delta p
