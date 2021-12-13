{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Foldable (foldMap', foldl')
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

data Fold = XFold Int | YFold Int deriving (Show)

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
positionParser = Pos <$> decimal <* char ',' <*> decimal <* eol

mapParser :: Parser (Map Position ())
mapParser = M.unions . map (\k -> M.singleton k ()) <$> many positionParser

foldParser :: Parser Fold
foldParser = do
    string "fold along "
    direction <*> (char '=' *> decimal) <* eol
  where
    direction :: Parser (Int -> Fold)
    direction = (XFold <$ char 'x') <|> (YFold <$ char 'y')

problemParser :: Parser (Map Position (), [Fold])
problemParser = do
    posMap <- mapParser
    eol
    folds <- many foldParser
    return (posMap, folds)

getMaxPos :: Map Position v -> (Int, Int)
getMaxPos posMap = (xMax, yMax)
  where
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys posMap

foldPosMap :: Semigroup v => Map Position v -> Fold -> Map Position v
foldPosMap coords foldAxis = M.unionWith (<>) firstHalf $ M.mapKeys foo secondHalf
  where
    (firstHalf, secondHalf) = M.partitionWithKey splitPred coords

    foo :: Position -> Position
    foo (Pos x y) = case foldAxis of
        XFold n -> Pos (n - (x-n)) y
        YFold n -> Pos x (n - (y-n))

    splitPred :: Position -> a -> Bool
    splitPred (Pos x y) _ = case foldAxis of
        XFold n -> x <= n
        YFold n -> y <= n

printMap_ :: Show v => Map Position v -> IO ()
printMap_ = printMap "." show

printMap :: String -> (v -> String) -> Map Position v -> IO ()
printMap def conv coords = do
    forM_ [0..yMax] $ \y -> do
        forM_ [0..xMax] $ \x -> do
            putStr . maybe def conv $ M.lookup (Pos x y) coords
        putStrLn ""
    putStrLn ""
  where
    (xMax, yMax) = getMaxPos coords

main :: IO ()
main = do
    args <- getArgs
    (posMap, (firstFold:folds)) <- case args of
        [inputFile] -> parseFile inputFile (problemParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let firstFoldMap = foldPosMap posMap firstFold
        foldedMap = foldl' foldPosMap firstFoldMap folds

    print (firstFold:folds)

    print (M.size firstFoldMap)
    printMap " " (const "#") $ foldedMap
