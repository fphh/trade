
module Trade.Render.Svg.DrawingVec where


import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

class DrawingVec vec where
  vecFromList :: [a] -> vec a
  vecToList :: vec a -> [a]
  vecHead :: vec a -> a
  vecLast :: vec a -> a
  vecFoldl :: (a -> b -> a) -> a -> vec b -> a
  vecConcat :: [vec a] -> vec a
  vecMap :: (a -> b) -> vec a -> vec b
  vecZipWith :: (a -> b -> c) -> vec a -> vec b -> vec c
  vecNull :: vec a -> Bool

instance DrawingVec [] where
  vecFromList = id
  vecToList = id
  vecHead = head
  vecLast = last
  vecFoldl = List.foldl'
  vecConcat = concat
  vecMap = map
  vecZipWith = zipWith
  vecNull = null

instance DrawingVec Vector where
  vecFromList = Vec.fromList
  vecToList = Vec.toList
  vecHead = Vec.head
  vecLast = Vec.last
  vecFoldl = Vec.foldl'
  vecConcat = Vec.concat
  vecMap = Vec.map
  vecZipWith = Vec.zipWith
  vecNull = Vec.null
  
