{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, manyTill)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data RPSMove = Rock | Paper | Scissors deriving (Eq, Show)

basicMove :: Parser RPSMove
basicMove = asum
    [ Rock <$ char 'A'
    , Rock <$ char 'X'
    , Paper <$ char 'B'
    , Paper <$ char 'Y'
    , Scissors <$ char 'C'
    , Scissors <$ char 'Z'
    ]

gameParser :: Parser (RPSMove, RPSMove)
gameParser = (,) <$> basicMove <* char ' ' <*> basicMove <* eol

gameResult :: (RPSMove, RPSMove) -> Int
gameResult game@(opponent, me)
    | opponent == me = 3
    | otherwise = case game of
        (Rock, Paper) -> 6
        (Paper, Scissors) -> 6
        (Scissors, Rock) -> 6
        _ -> 0

shapeValue :: RPSMove -> Int
shapeValue Rock = 1
shapeValue Paper = 2
shapeValue Scissors = 3

gameValue :: (RPSMove, RPSMove) -> Int
gameValue game@(_, myShape) = gameResult game + shapeValue myShape

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (manyTill gameParser eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . sum $ map gameValue inputData
