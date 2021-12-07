module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
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

crabPosToCrabVector :: [Int] -> Vector Int
crabPosToCrabVector crabPos = case IM.lookupMax posCounts of
    Nothing -> VU.empty
    Just (k, _) -> VU.generate (k+1) lookupCount
  where
    posCounts :: IntMap Int
    posCounts = IM.fromListWith (+) $ map (\k -> (k,1)) crabPos

    lookupCount :: Int -> Int
    lookupCount n = fromMaybe 0 $ IM.lookup n posCounts

crabsParser :: Parser (Vector Int)
crabsParser = crabPosToCrabVector <$> sepBy1 decimal (char ',') <* eol

computeAlignmentCosts :: (Vector Int -> Int -> Int) -> Vector Int -> Vector Int
computeAlignmentCosts align vec = VU.generate (VU.length vec) (align vec)

alignPosition1 :: Vector Int -> Int -> Int
alignPosition1 vec idx = VU.ifoldl' costPerPos 0 vec
  where
    costPerPos :: Int -> Int -> Int -> Int
    costPerPos acc i v = acc + abs (i - idx) * v

alignPosition2 :: Vector Int -> Int -> Int
alignPosition2 vec idx = VU.ifoldl' costPerPos 0 vec
  where
    costPerPos :: Int -> Int -> Int -> Int
    costPerPos acc i v = acc + cost * v
      where
        offset :: Double
        offset = fromIntegral $ abs (i - idx)

        cost :: Int
        cost = round $ (offset + 1) * (offset/2)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (crabsParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . VU.minimum $ computeAlignmentCosts alignPosition1 inputData
    print . VU.minimum $ computeAlignmentCosts alignPosition2 inputData
