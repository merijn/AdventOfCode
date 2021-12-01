module Main where

import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

toValues :: Text -> Either String [Int]
toValues = traverse toNum . T.lines
  where
    toNum :: Text -> Either String Int
    toNum txt = case T.decimal txt of
        Left t -> Left t
        Right (i, remainder)
            | T.null remainder -> Right i
            | otherwise -> Left "Parse error!"

solve :: Ord a => [a] -> Int
solve [] = 0
solve (x:xs) = snd $ foldl' go (x, 0) xs
  where
    go :: Ord a => (a, Int) -> a -> (a, Int)
    go (v1, n) v2
        | v1 < v2 = (v2, n+1)
        | otherwise = (v2, n)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case solve <$> toValues inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right incs -> putStrLn $ show incs ++ " increments"
