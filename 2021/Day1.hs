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

countIncrements :: Ord a => [a] -> Int
countIncrements [] = 0
countIncrements (x:xs) = snd $ foldl' go (x, 0) xs
  where
    go :: Ord a => (a, Int) -> a -> (a, Int)
    go (v1, n) v2
        | v1 < v2 = (v2, n+1)
        | otherwise = (v2, n)

slidingWindow :: (Ord a, Num a) => [a] -> [a]
slidingWindow (x1:xs@(x2:x3:_)) = x1 + x2 + x3 : slidingWindow xs
slidingWindow _ = []

zipSlidingWindow :: (Ord a, Num a) => [a] -> [a]
zipSlidingWindow xs = zipWith3 add3 xs (drop 1 xs) (drop 2 xs)
  where
    add3 :: Num a => a -> a -> a -> a
    add3 x y z = x + y + z

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case toValues inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right values -> do
            let valueIncrements = countIncrements values
                windowIncrements = countIncrements $ slidingWindow values
                windowIncrements2 = countIncrements $ zipSlidingWindow values

            putStrLn $ show valueIncrements ++ " value increments"
            putStrLn $ mconcat
                [ show windowIncrements, " (", show windowIncrements2, ") "
                , " window increments"
                ]
