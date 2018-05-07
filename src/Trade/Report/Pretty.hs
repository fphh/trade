{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Report.Pretty where

import qualified Data.List as List

import Data.Time.Clock (UTCTime, NominalDiffTime)

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Integer where
  pretty = show

instance Pretty Double where
  pretty = show

instance Pretty UTCTime where
  pretty = show

instance Pretty NominalDiffTime where
  pretty = show

instance Pretty String where
  pretty = id

instance (Pretty a) => Pretty (Maybe a) where
  pretty Nothing = "N/A"
  pretty (Just x) = pretty x

instance (Pretty a) => Pretty [a] where
  pretty = List.intercalate ", " . map pretty
