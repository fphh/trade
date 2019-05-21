{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Report.ToReport where

-- import Control.Monad

import Control.Monad.Reader (ReaderT(..))

import Text.Blaze.Html5 (Html)

import qualified Trade.Report.Report as Rep
import Trade.Report.Config (Config, HtmlReader)

import Data.Monoid ((<>), mempty)


class ToReport a where
  toReport :: a -> HtmlReader ()


instance ToReport () where
  toReport _ = ReaderT (const mempty)

{-
instance (ToReport a) => ToReport (Maybe a) where
  toReport = maybe (toReport ()) toReport
-}

instance (ToReport a) => ToReport [a] where
  toReport = fmap mconcat . mapM toReport

instance ToReport Html where
  toReport = ReaderT . const
  
