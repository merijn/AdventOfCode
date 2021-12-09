{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(..), Min(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, many, manyTill)
import Text.Megaparsec.Char (digitChar, eol)
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

rowParser :: Parser (Int -> Map Position Int)
rowParser = mconcat . zipWith mkPosMap [0..] <$> manyTill digitParser eol
  where
    digitParser :: Parser Int
    digitParser = digitToInt <$> digitChar

    mkPosMap :: Int -> v -> Int -> Map Position v
    mkPosMap x v y = M.singleton (Pos x y) v


mapParser :: Parser (Map Position Int)
mapParser = M.unions . zipWith (&) [0..] <$> many rowParser

printMap :: Map Position Int -> IO ()
printMap coords = do
    forM [0..yMax] $ \y -> do
        forM [0..xMax] $ \x -> do
            putStr . maybe "." show $ M.lookup (Pos x y) coords
        putStrLn ""
    putStrLn ""
  where
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys coords

findSinks :: Map Position Int -> Map Position Int
findSinks = coerce $ mapConvolution localMinimum
  where
    localMinimum :: Min Int -> [Min Int] -> Maybe (Min Int)
    localMinimum curr vs
        | mconcat vs <= curr = Nothing
        | otherwise = Just curr

mapConvolution :: (v -> [v] -> Maybe v) -> Map Position v -> Map Position v
mapConvolution convolve posMap = M.mapMaybeWithKey updatePosition posMap
  where
    updatePosition (Pos x y) v = convolve v $ mapMaybe (`M.lookup` posMap)
        [ Pos (x-1) y
        , Pos (x+1) y
        , Pos x (y-1)
        , Pos x (y+1)
        ]

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (mapParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let riskMap = (+1) <$> findSinks inputData
    printMap inputData
    printMap riskMap
    print $ sum riskMap
