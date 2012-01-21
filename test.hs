module Main where

import Text.Pandoc
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.IO
import System.FilePath


markdownToHtml :: String -> String
markdownToHtml =
  (writeHtmlString defaultWriterOptions) . readMarkdown defaultParserState


publishIndex :: IO ()
publishIndex = do
  withFile "index.md" WriteMode $ \h -> do
    getSectionDirs >>= mapM_ (hPutStrLn h)

visibleFiles :: [[Char]] -> [[Char]]
visibleFiles = filter (not . isPrefixOf ".")

markdownFiles :: [[Char]] -> [[Char]]
markdownFiles = filter (isSuffixOf ".md")

getSectionDirs :: IO [[Char]]
getSectionDirs = do
  tree <- getDirectoryContents "_posts"
  return $ visibleFiles tree

publishSection section = do
  files <- getDirectoryContents $ sectionPath 
  mapM_ putStrLn $ visibleFiles files
  where
    sectionPath = combine "_posts" section

publishAllFiles = do
  sections <- getSectionDirs
  sections <- filterM (doesDirectoryExist . combine "_posts") sections
  mapM_ publishSection $ sections



main = publishAllFiles
