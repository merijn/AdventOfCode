{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (Max(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, many)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Position
    = Pos
      { columnPos :: {-# UNPACK #-} !Int
      , rowPos :: {-# UNPACK #-} !Int
      } deriving (Show)

instance Eq Position where
    Pos x1 y1 == Pos x2 y2 = x1 == x2 && y1 == y2

instance Ord Position where
    compare (Pos x1 y1) (Pos x2 y2) = compare x1 x2 <> compare y1 y2

positionParser :: Parser Position
positionParser = Pos <$> decimal <* char ',' <*> decimal

lineParser :: Parser (Map Position Int)
lineParser = do
    startPos <- positionParser
    string " -> "
    endPos <- positionParser <* eol
    return . M.fromList . map (\v -> (v,1)) $ computeLine startPos endPos
  where
    computeLine :: Position -> Position -> [Position]
    computeLine (Pos x1 y1) (Pos x2 y2)
        | x1 == x2 = map (Pos x1) [min y1 y2 .. max y1 y2]
        | y1 == y2 = map (\x -> Pos x y1) [min x1 x2 .. max x1 x2]
        | otherwise = []

mapParser :: Parser (Map Position Int)
mapParser = M.unionsWith (+) <$> many lineParser

printMap :: Map Position Int -> IO ()
printMap coords = do
    forM [0..xMax] $ \y -> do
        forM [0..yMax] $ \x -> do
            putStr . maybe "." show $ M.lookup (Pos x y) coords
        putStrLn ""
    putStrLn ""
  where
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys coords

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (mapParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let overlappingCount = M.size (M.filter (>=2) inputData)

    putStrLn $ "Overlap >2: " ++ show overlappingCount
