module Main where

import Control.Monad (replicateM)
import Data.Foldable (asum)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, many, some)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Segment = A | B | C | D | E | F | G
    deriving (Eq, Ord, Show)

newtype Digit = Digit { digitSegments :: Set Segment }
    deriving (Eq, Ord, Show)

segmentParser :: Parser Segment
segmentParser = asum
    [ A <$ char 'a', B <$ char 'b', C <$ char 'c', D <$ char 'd'
    , E <$ char 'e', F <$ char 'f', G <$ char 'g'
    ]

digitParser :: Parser Digit
digitParser = Digit . S.fromList <$> some segmentParser

displayParser :: Parser ([Digit], [Digit])
displayParser = do
    patterns <- replicateM 10 (digitParser <* char ' ')
    char '|'
    display <- replicateM 4 (char ' ' *> digitParser) <* eol
    return (patterns, display)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (many displayParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let outputDigits = concatMap snd inputData
        isUniqueDigit (Digit s) = S.size s `elem` [2,3,4,7]

    print . length $ filter isUniqueDigit outputDigits
