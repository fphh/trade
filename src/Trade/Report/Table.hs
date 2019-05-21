

module Trade.Report.Table where

import Control.Monad.Reader (ReaderT(..))

import qualified Data.List as List

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A


-- import Trade.Report.HtmlIO (HtmlIO, HtmlT(..))
import Trade.Report.Config (HtmlReader)

seperator :: Char -> Int -> Int -> [String]
seperator c width n = replicate n (replicate width c)

boldSep :: Int -> Int -> [String]
boldSep = seperator '='


sep :: Int -> Int -> [String]
sep = seperator '-'

tableRows :: [[String]] -> [String]
tableRows rs =
  let rowLens = map length rs
      maxRowLen = maximum rowLens
      -- f len r = r ++ replicate (maxRowLen - length r) ""
      -- filledRows = zipWith f rowLens rs

      f r = r ++ replicate (maxRowLen - length r) ""
      filledRows = map f rs

      rightPadding = 8
      
      g col =
        let m = maximum (map length col)
            h c = replicate (m - length c + rightPadding) ' ' ++ c
        in map h col
      filledCols = map g (List.transpose filledRows)

      filledCells = List.transpose filledCols
      filledCellsLen = length filledCells - 1

      grid c xs = c : concatMap (\x -> [x, c]) xs

      isEmpty = all (== ' ')

      bold True = '='
      bold False = '-'
      
      i rn row
        | all isEmpty row = grid "+" (map (map (const (bold (rn == 0 || rn == filledCellsLen)))) row)
        | otherwise = grid "|" row
      
      es = zipWith i [0..] filledCells
      
  in map concat es


table :: [[String]] -> HtmlReader ()
table rs =
  let rows = tableRows ([""] : rs ++[[""]])
      cbSty = H5A.style (H5.stringValue "clear:both")
      tbSty = H5A.style (H5.stringValue "margin-bottom:12px")
      g ' ' = "&nbsp;"
      g c = [c]
      f = (H5.div ! cbSty) . H5.preEscapedToHtml . concatMap g
  in ReaderT (const ((H5.div ! tbSty) (mapM_ f rows)))

tableList :: [[[String]]] -> HtmlReader ()
tableList = sequence_ . map table
