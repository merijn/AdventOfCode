module Main where

import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector, (!))
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

fishDaysToPopVector :: [Int] -> Vector Int
fishDaysToPopVector fishDays = VU.generate 9 lookupCount
  where
    dayCounts :: IntMap Int
    dayCounts = IM.fromListWith (+) $ map (\k -> (k,1)) fishDays

    lookupCount :: Int -> Int
    lookupCount n = fromMaybe 0 $ IM.lookup n dayCounts

lanternFishParser :: Parser (Vector Int)
lanternFishParser = fishDaysToPopVector <$> sepBy1 decimal (char ',') <* eol

evalPopulation :: Vector Int -> Int -> Vector Int
evalPopulation v days = foldl' updatePopulation v [1..days]
  where
    updatePopulation :: Vector Int -> a -> Vector Int
    updatePopulation population _ = VU.generate 9 lookupPop
      where
        lookupPop :: Int -> Int
        lookupPop 8 = population ! 0
        lookupPop 6 = (population ! 0) + (population ! 7)
        lookupPop n = population ! (n + 1)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (lanternFishParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . VU.sum $ evalPopulation inputData 80
    print . VU.sum $ evalPopulation inputData 256
