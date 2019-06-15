

module Trade.Report.Sample where

import Control.Monad (zipWithM_)

import System.FilePath.Posix ((</>))


import qualified Data.ByteString.Lazy as BSL


import System.Directory (createDirectory, removeDirectoryRecursive)

import Trade.Report.HtmlReader (HtmlReader, render)

renderToDirectory :: FilePath -> [HtmlReader ()] -> IO ()
renderToDirectory dir xs = do
  removeDirectoryRecursive dir
  createDirectory dir
  let m i x = do
        t <- render x
        BSL.writeFile (dir </> "analysis-" ++ show i ++ ".html") t
  zipWithM_ m [0 :: Int ..] xs


