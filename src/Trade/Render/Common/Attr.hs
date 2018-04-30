{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Common.Attr where


import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Monoid

import Trade.Common.Color

data Attr = Attr String String deriving (Show)

class (Show a) => AttrPair a where
  (.=) :: String -> a -> Attr
  x .= y = Attr x (show y)

instance AttrPair String where
  (.=) = Attr
  
instance AttrPair Integer

instance AttrPair Int

instance AttrPair Double

instance AttrPair Color

type Attrs = Map String String

emptyAttrs :: Attrs
emptyAttrs = Map.empty

toAs :: [Attr] -> Attrs
toAs = Map.fromList . map (\(Attr x y) -> (x, y))

colon :: Builder
colon = B.charUtf8 ':'

semicolon :: Builder
semicolon = B.charUtf8 ';'

attr2str :: Attrs -> Builder
attr2str as = B.stringUtf8 " style=\"" <> Map.foldrWithKey f mempty as <> B.stringUtf8 "\" "
  where f k v acc = B.stringUtf8 k <> colon <> B.stringUtf8 v <> semicolon <> acc

findRemove :: (Ord a) => a -> Map a b -> (Maybe b, Map a b)
findRemove = Map.updateLookupWithKey (\_ _ -> Nothing)

getClassAndId :: Attrs -> (String, String, Attrs)
getClassAndId as =
  let g = maybe "" id
      mapV f (x, y) = (f x, y)
      (i, bs) = mapV g (findRemove "id" as)
      (c, cs) = mapV g (findRemove "class" bs)
  in (i, c, cs)
