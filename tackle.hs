module Main where

import Text.Pandoc
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.IO
import System.FilePath

import Control.Applicative ((<$>), (<*>), (<*))
import qualified Data.Map as M

import Data.Char (isSpace)

import Text.Parsec.Char (alphaNum, anyChar, char, newline, oneOf, string)
import Text.Parsec.Combinator (choice, many1, manyTill, option, skipMany1)
import Text.Parsec.Prim (many, parse, skipMany, (<?>))
import Text.Parsec.String (Parser)

trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace


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


simpleParse :: [String] -> [String]
simpleParse ("---":xs) = simpleParse xs
simpleParse (x:xs) = x : simpleParse xs
simpleParse [] = []

inlineSpace :: Parser Char
inlineSpace = oneOf ['\t', ' '] <?> "space"

metadataField :: Parser (String, String)
metadataField = do
  key <- manyTill alphaNum $ char ':'
  skipMany inlineSpace
  value <- manyTill anyChar newline
  trailing' <- many trailing
  return (key, trim $ value ++ concat trailing')
  where
    trailing = (++) <$> many1 inlineSpace <*> manyTill anyChar newline

-- YAML frontmatter
metadata :: Parser [(String, String)]
metadata = do
  open <- many1 (char '-') <* many inlineSpace <* newline
  metadata' <- many metadataField
  string open
  skipMany inlineSpace
  skipMany1 newline
  return metadata'

file :: Parser ([(String, String)], String)
file = do
  metadata' <- option [] metadata
  body <- many anyChar
  return (metadata', body)

parseFile :: String -> ([(String, String)], String)
parseFile input = case parse file "page" input of
  Left err -> error (show err)
  Right (md, bd) -> (md, bd)

readTemplate :: FilePath -> IO String
readTemplate f = do
  ex <- doesFileExist f
  if ex
    then readFile f
    else return "$body$"

publishFile meta path = do
  fd <- readFile path
  let (md, bd) = parseFile fd
  let meta' = meta ++ md
  tmplName <- case lookup "layout" meta' of
    Just val -> return ("_layouts" </> val <.> "html")
    Nothing -> return ("_layouts" </> "default.html")
  tmpl <- readTemplate tmplName
  putStrLn $ 
    writeHtmlString defaultWriterOptions { writerStandalone = True, 
                                           writerTemplate = tmpl, 
                                           writerVariables = meta' } $ 
    readMarkdown defaultParserState bd


publishSection meta section = do
  files <- getDirectoryContents $ sectionPath 
  let files' = visibleFiles files
  let meta' = meta ++ map (\d -> (section, d)) files'
  mapM_ (publishFile meta') $ map (combine sectionPath) $ files'
  where
    sectionPath = combine "_posts" section

publishAllFiles = do
  sections <- getSectionDirs
  sections <- filterM (doesDirectoryExist . combine "_posts") sections
  mapM_ (publishSection []) sections

main = publishAllFiles

       