

module Trade.Render.Common.Utils where

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

import Data.Monoid

tag :: Builder -> Builder -> Builder -> Builder -> Builder
tag open close attrs inner =
  open <> attrs <> (B.charUtf8 '>') <> inner <> close


leq, geq, cls :: Builder
leq = B.charUtf8 '<'
geq = B.charUtf8 '>'
cls = B.stringUtf8 "</"

tag2 :: String -> Builder -> Builder -> Builder
tag2 tagg attrs inner =
  let t = B.stringUtf8 tagg
  in leq <> t <> attrs <> geq <> inner <> cls <> t <> geq
  
