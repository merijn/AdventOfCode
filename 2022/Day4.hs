module Main where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, some)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Section = Section Int Int deriving (Show)

sectionParser :: Parser Section
sectionParser = Section <$> decimal <* char '-' <*> decimal

sectionPairParser :: Parser (Section, Section)
sectionPairParser = (,) <$> sectionParser <* char ',' <*> sectionParser <* eol

fullyContains :: Section -> Section -> Bool
fullyContains (Section l1 h1) (Section l2 h2)
    | l1 <= l2 && h1 >= h2 = True
    | l2 <= l1 && h2 >= h1 = True
    | otherwise = False

overlap :: Section -> Section -> Bool
overlap (Section l1 h1) (Section l2 h2) = not . S.null $
  S.fromAscList [l1 .. h1] `S.intersection` S.fromAscList [l2 .. h2]

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (some sectionPairParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    putStrLn "Puzzle #1:"
    print . length $ filter (uncurry fullyContains) inputData
    putStrLn "Puzzle #2:"
    print . length $ filter (uncurry overlap) inputData
