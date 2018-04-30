{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}


module Trade.Render.Svg.Svg where

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

import Trade.Render.Common.Attr
import Trade.Render.Common.Utils

comma :: Builder
comma = B.charUtf8 ','

space :: Builder
space = B.charUtf8 ' '

dot :: Builder
dot = B.charUtf8 '.'

precision :: Double
precision = 10

class ToPolyLine vec where
  toPolyLine :: (Show x) => vec (x, Double) -> Builder

instance ToPolyLine [] where
  toPolyLine ps = B.stringUtf8 "points=\"" <> List.foldr f mempty ps <> B.stringUtf8 "\" "
    where f (x, y) acc = pf x <> comma <> pf y <> space <> acc
          --pf c = B.intDec (round c)
          pf c = B.string8 (show c) -- B.intDec c

instance ToPolyLine Vector where
  toPolyLine ps = B.stringUtf8 "points=\"" <> Vec.foldr' f mempty ps <> B.stringUtf8 "\" "
    where f (x, y) acc = pf x <> comma <> pf y <> space <> acc
          -- pf c = B.intDec (round c)
          pf c = B.string8 (show c)

class Svg2String a where
  svg2str :: a -> Builder

instance Svg2String a => Svg2String [a] where
  svg2str = mconcat . map svg2str
  
data Svg vec = Svg Attrs [Drawing vec] -- deriving (Show)

svg :: Builder -> Builder -> Builder
svg = tag (B.stringUtf8 "<svg ") (B.stringUtf8 "</svg>")

attr2b :: (String, String) -> Builder
attr2b (k, v) = B.stringUtf8 k <> B.stringUtf8 "=\"" <> B.stringUtf8 v <> B.stringUtf8 "\" "

instance (ToPolyLine vec) => Svg2String (Svg vec) where
  svg2str (Svg as drawing) =
    let (Just w, bs) = findRemove "width" as
        (Just h, cs) = findRemove "height" bs

        
        (i, c, ds) = getClassAndId cs
        xml = B.stringUtf8 "<?xml version=\"1.0\"?>"
        attrs = map attr2b $
          (if null i then [] else [("id", i)])
          ++
          (if null c then [] else [("class", c)])
          ++
          [ ("width", w), ("height", h)
          , ("viewbox", "0 0 " ++ w ++ " " ++ h)
          , ("xmlns", "http://www.w3.org/2000/svg")
          , ("version", "1.1") ]
    in xml <> svg (mconcat attrs <> attr2str ds) (svg2str drawing)



type Translate = (Double, Double)

data Drawing vec =
  G Attrs Translate [Drawing vec]
  | PolyLine Attrs (vec (Double, Double))
  | Circle Attrs Double (Double, Double) String
  | Text Attrs (Double, Double) String
  | Empty
  -- deriving (Show)


g_ :: Attrs -> [Drawing vec] -> Drawing vec
g_ = flip G (0, 0)


instance (ToPolyLine vec) => Svg2String (Drawing vec) where
  
  svg2str (G as cs ds) =
    let (i, c, bs) = getClassAndId as
        attrs = map attr2b $
          (if null i then [] else [("id", i)])
          ++
          (if null c then [] else [("class", c)])
          ++
          [ ("transform", "translate" ++ show cs) ]

        -- rect = B.stringUtf8 "<rect x='0' y='0' width='1040' height='440' style='stroke:black;stroke-width:1px;fill:yellow;opacity:0.2'></rect>"
    
    in tag (B.stringUtf8 "<g ") (B.stringUtf8 "</g>") (mconcat attrs <> attr2str bs) ({- rect <> -} svg2str ds)

  svg2str (PolyLine as ps) =
    let (i, c, bs) = getClassAndId as 
        attrs = map attr2b $
          (if null i then [] else [("id", i)])
          ++
          (if null c then [] else [("class", c)])
          ++
          []
        vs = mconcat attrs <> attr2str bs <> toPolyLine ps
    in tag (B.stringUtf8 "<polyline ") (B.stringUtf8 "</polyline>") vs mempty

  svg2str (Circle as r (x, y) txt) =
    let (i, c, bs) = getClassAndId as     
        attrs = map attr2b $
          (if null i then [] else [("id", i)])
          ++
          (if null c then [] else [("class", c)])
          ++
          [ ("cx", show x), ("cy", show y), ("r", show r) ]
        title = tag (B.stringUtf8 "<title ") (B.stringUtf8 "</title>") mempty (B.stringUtf8 txt)
    in tag (B.stringUtf8 "<circle ") (B.stringUtf8 "</circle>") (mconcat attrs <> attr2str bs) title

  svg2str (Text as (x, y) str) =
    let (i, c, bs) = getClassAndId as
        attrs = map attr2b $
          (if null i then [] else [("id", i)])
          ++
          (if null c then [] else [("class", c)])
          ++ [("x", show x), ("y", show y)]
    in tag (B.stringUtf8 "<text ") (B.stringUtf8 "</text>") (mconcat attrs <> attr2str bs) (B.stringUtf8 str)

  svg2str Empty = B.stringUtf8 ""
