{-# LANGUAGE UndecidableInstances #-}


module Trade.Report.HtmlIO where


import Control.Monad (liftM, ap)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO, lift)

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlBSL
import qualified Text.Blaze.Html5.Attributes as H5A
import Text.Blaze.Internal (MarkupM(..))



markupValue :: MarkupM a -> a
markupValue m0 = case m0 of
  Parent _ _ _ m1           -> markupValue m1
  CustomParent _ m1         -> markupValue m1
  Leaf _ _ _ x              -> x
  CustomLeaf _ _ x          -> x
  Content _ x               -> x
  Comment _ x               -> x
  Append _ m1               -> markupValue m1
  AddAttribute _ _ _ m1     -> markupValue m1
  AddCustomAttribute _ _ m1 -> markupValue m1
  Empty x                   -> x

newtype HtmlT m a = HtmlT { runHtmlT :: m (MarkupM a) }

instance (Monad m) => Functor (HtmlT m) where
  fmap = liftM

instance (Monad m) => Applicative (HtmlT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (HtmlT m) where
  return = HtmlT . return . Empty

  -- (>>=) :: HtmlT m a -> (a -> HtmlT m b) -> HtmlT m b
  x >>= f = HtmlT $ do
    y <- runHtmlT x
    z <- runHtmlT (f (markupValue y))
    return (Append y z)
    
instance (Monoid (m (MarkupM a))) => Monoid (HtmlT m a) where
  mempty = HtmlT mempty
  mappend (HtmlT x) (HtmlT y) = HtmlT (mappend x y)
  mconcat = HtmlT . mconcat . map runHtmlT

instance (Monoid (m (MarkupM a))) => Semigroup (HtmlT m a) where
  (<>) = mappend

instance MonadTrans HtmlT where
    lift = HtmlT . (liftM Empty)

instance MonadIO m => MonadIO (HtmlT m) where
    liftIO = lift . liftIO

liftHtml :: (MarkupM a -> MarkupM b) -> HtmlT IO a -> HtmlT IO b
liftHtml f (HtmlT h) = HtmlT $ h >>= return . f

type HtmlIO = HtmlT IO ()


class ToHtmlIO a where
  toHtmlIO :: a -> HtmlIO
