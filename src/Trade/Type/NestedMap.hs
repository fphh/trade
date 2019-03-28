

module Trade.Type.NestedMap where

import qualified Data.Map as Map
import Data.Map (Map)


newtype NestedMap k0 k1 v = NestedMap {
  unNestedMap :: Map k0 (Map k1 v)
  } deriving (Show)


instance Functor (NestedMap k0 k1) where
  fmap f (NestedMap mp) = NestedMap (fmap (fmap f) mp)

fold :: (Monoid m) => (k0 -> k1 -> v -> m) -> NestedMap k0 k1 v -> m
fold f (NestedMap mp) = Map.foldMapWithKey (Map.foldMapWithKey . f) mp

union :: (Ord k0, Ord k1, Monoid v) => NestedMap k0 k1 v -> NestedMap k0 k1 v -> NestedMap k0 k1 v
union (NestedMap m0) (NestedMap m1) = NestedMap (Map.unionWith (Map.unionWith (<>)) m0 m1)


-- TODO: use merge or mergeA ?
zipWith ::
  (Ord k0, Ord k1) =>
  (k0 -> k1 -> x -> y -> z) -> NestedMap k0 k1 x -> NestedMap k0 k1 y -> NestedMap k0 k1 z
zipWith f (NestedMap m0) (NestedMap m1) =
  let err = error "NestedMap.zipWith: missing key"
      g k0 k1 x = maybe err (f k0 k1 x) (Map.lookup k0 m1 >>= Map.lookup k1)
  in NestedMap (Map.mapWithKey (Map.mapWithKey . g) m0)
