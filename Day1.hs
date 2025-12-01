module Main (main) where

import Data.Foldable (forM_)
import Data.List (mapAccumL, scanl')
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

parseTurn :: String -> Maybe Int
parseTurn s = case s of
    ('L':rest) -> negate <$> readMaybe rest
    ('R':rest) -> readMaybe rest
    _ -> Nothing

day1_1 :: [Int] -> [Int]
day1_1 = scanl' turn 50
  where
    turn s offset = (s + offset) `mod` 100

day1_2 :: [Int] -> Int
day1_2 = sum . snd . mapAccumL turn 50
  where
    turn s offset = (result, abs fullTurns + rest)
      where
        result = (s + offset) `mod` 100

        (fullTurns, remainder) = offset `quotRem` 100

        rest | s == 0 = 0
             | s + remainder >= 100 = 1
             | s + remainder <= 0 = 1
             | otherwise = 0


main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let optInstructions = traverse parseTurn (lines inputData)
    forM_ optInstructions $ print . length . filter (== 0) . day1_1
    forM_ optInstructions $ print . day1_2
