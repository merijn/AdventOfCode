{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (asum, foldl')
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, manyTill)
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

data SubCommand = Forward Int | Up Int | Down Int deriving (Show)

commandParser :: Parser SubCommand
commandParser = command <* char ' ' <*> decimal <* eol
  where
    command = asum
        [ Forward <$ string "forward"
        , Up <$ string "up"
        , Down <$ string "down"
        ]

updatePosition :: (Int, Int) -> SubCommand -> (Int, Int)
updatePosition (!x, !y) cmd = case cmd of
    Forward n -> (x + n, y)
    Up n -> (x, y - n)
    Down n -> (x, y + n)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (manyTill commandParser eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let position = foldl' updatePosition (0, 0) inputData
    print $ uncurry (*) position
