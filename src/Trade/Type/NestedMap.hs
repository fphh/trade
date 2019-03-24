

module Trade.Type.NestedMap where

import qualified Data.Map as Map
import Data.Map (Map)

newtype NestedMap k0 k1 v = NestedMap {
  unNestedMap :: Map k0 (Map k1 v)
  } deriving (Show)


fold :: (Monoid m) => (k0 -> k1 -> v -> m) -> NestedMap k0 k1 v -> m
fold f (NestedMap mp) = Map.foldMapWithKey (Map.foldMapWithKey . f) mp

union :: (Ord k0, Ord k1, Monoid v) => NestedMap k0 k1 v -> NestedMap k0 k1 v -> NestedMap k0 k1 v
union (NestedMap m0) (NestedMap m1) = NestedMap (Map.unionWith (Map.unionWith (<>)) m0 m1)
