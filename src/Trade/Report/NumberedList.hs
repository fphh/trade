{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Report.NumberedList where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Report.Pretty


class ToNumberedLine a where
  toNumberedLine :: Int -> a -> [String]

instance (Pretty a, Pretty b) => ToNumberedLine (a, b) where
  toNumberedLine i (x, y) = [pretty i, pretty x, pretty y]

class ToNumberedList vec where
  toNumberedList :: vec -> [[String]]
  toNumberedListN :: Int -> vec -> [[String]]
  toNumberedListN n = take n . toNumberedList

instance (ToNumberedLine a) => ToNumberedList (Vector a) where
  toNumberedList = Vec.toList . Vec.imap toNumberedLine

instance (ToNumberedLine a) => ToNumberedList [a] where
  toNumberedList = zipWith toNumberedLine [0..]
    

