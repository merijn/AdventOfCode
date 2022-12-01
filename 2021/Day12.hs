{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isUpper)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, some)
import Text.Megaparsec.Char (char, eol, letterChar)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

tunnelParser :: Parser (Map Text (Set Text))
tunnelParser = do
    origin <- T.pack <$> some letterChar <* char '-'
    destination <- T.pack <$> some letterChar <* eol
    return $ M.fromListWith (<>)
        [(origin, S.singleton destination), (destination, S.singleton origin)]

caveParser :: Parser (Map Text (Set Text))
caveParser = M.unionsWith (<>) <$> some tunnelParser

isSmallCave :: Text -> Bool
isSmallCave = not . T.all isUpper

findPaths :: Map Text (Set Text) -> [[Text]]
findPaths caveSystem = go caveSystem [] "start"
  where
    go :: Map Text (Set Text) -> [Text] -> Text -> [[Text]]
    go _ path "end" = [reverse ("end":path)]
    go caves path name = case M.lookup name caves of
        Nothing -> []
        Just dests -> concatMap (go subCaves (name:path)) $ S.toList dests
      where
        subCaves | isSmallCave name = M.delete name caves
                 | otherwise = caves

findPaths2 :: Map Text (Set Text) -> Set [Text]
findPaths2 caveSystem = S.fromList $ go caveSystem [] False "start"
  where
    go :: Map Text (Set Text) -> [Text] -> Bool -> Text -> [[Text]]
    go _ path _ "end" = [reverse ("end":path)]
    go caves path noDoubleVisit name
        | smallCave && inPath && noDoubleVisit = []
        | otherwise = case M.lookup name caves of
            Nothing -> []
            Just dests -> computePaths $ S.toList dests
      where
        smallCave = isSmallCave name
        inPath = name `elem` path
        newNoDoubleVisit = (smallCave && inPath) || noDoubleVisit

        computePaths = concatMap (go subCaves (name:path) newNoDoubleVisit)

        removeCave
            | name == "start" || name == "end" = True
            | smallCave && newNoDoubleVisit = True
            | otherwise = False

        subCaves | removeCave = M.delete name caves
                 | otherwise = caves

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (caveParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print $ length (findPaths inputData)
    print $ S.size (findPaths2 inputData)
