module Main where

import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, sepBy1)
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

lanternFishParser :: Parser (Vector Int)
lanternFishParser = VU.fromList <$> sepBy1 decimal (char ',') <* eol

evalPopulation :: Vector Int -> Int -> Vector Int
evalPopulation v days = foldl' updatePopulation v [1..days]
  where
    updatePopulation :: Vector Int -> a -> Vector Int
    updatePopulation population _ = newPopulation
      where
        newCount :: Int
        newCount = VU.length $ VU.elemIndices 0 population

        newPopulation :: Vector Int
        newPopulation = VU.map updatePop population <> VU.replicate newCount 8

        updatePop :: Int -> Int
        updatePop 0 = 6
        updatePop n = n - 1

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (lanternFishParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . VU.length $ evalPopulation inputData 80
