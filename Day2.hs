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

data GameResult = Lose | Draw | Win deriving (Eq, Show)

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

resultParser :: Parser GameResult
resultParser = asum
    [ Lose <$ char 'X'
    , Draw <$ char 'Y'
    , Win <$ char 'Z'
    ]

strategyParser :: Parser (RPSMove, GameResult)
strategyParser = (,) <$> basicMove <* char ' ' <*> resultParser <* eol

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

resultValue :: GameResult -> Int
resultValue result = case result of
    Lose -> 0
    Draw -> 3
    Win -> 6

moveToMake :: (RPSMove, GameResult) -> RPSMove
moveToMake (opponent, Draw) = opponent
moveToMake (opponent, Lose) = case opponent of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
moveToMake (opponent, Win) = case opponent of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

strategyValue :: (RPSMove, GameResult) -> Int
strategyValue strat@(_, result) =
    shapeValue (moveToMake strat) + resultValue result

main :: IO ()
main = do
    args <- getArgs
    (puzzle1, puzzle2) <- case args of
        [inputFile] -> do
            puzzle1 <- parseFile inputFile (manyTill gameParser eof)
            puzzle2 <- parseFile inputFile (manyTill strategyParser eof)
            return (puzzle1, puzzle2)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    putStr "Puzzle #1: "
    print . sum $ map gameValue puzzle1
    putStr "Puzzle #2: "
    print . sum $ map strategyValue puzzle2
