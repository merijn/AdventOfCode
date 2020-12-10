module Main(main) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

toValues :: Text -> Either String [Int]
toValues = mapM toSingleton . T.lines
  where
    toSingleton :: Text -> Either String Int
    toSingleton txt = case T.decimal txt of
        Left t -> Left t
        Right (t, remainder)
            | T.null remainder -> Right t
            | otherwise -> Left "Parse error!"

solveSum :: Int -> IntSet -> Maybe (Int, Int)
solveSum target allValues = do
    (minVal, maxValues) <- IS.minView allValues
    (maxVal, remaining) <- IS.maxView maxValues
    go minVal maxVal remaining
  where
    go :: Int -> Int -> IntSet -> Maybe (Int, Int)
    go low high vals
        | low + high < target = do
            (newMin, remaining) <- IS.minView vals
            go newMin high remaining

        | low + high > target = do
            (newMax, remaining) <- IS.maxView vals
            go low newMax remaining

        | otherwise = Just (low, high)

validateSequence :: Int -> [Int] -> Either Int ()
validateSequence preambleLen input = () <$
    foldM validateValue preamble (zip input rest)
  where
    (preamble, rest) = first IS.fromList $ splitAt preambleLen input

    validateValue :: IntSet -> (Int, Int) -> Either Int IntSet
    validateValue s (old, next) = case solveSum next s of
        Nothing -> Left next
        Just _ -> Right (IS.insert next (IS.delete old s))

findWeakness :: Int -> [Int] -> Maybe (Int, Int)
findWeakness n inputs = listToMaybe . mapMaybe trySequence $ tails inputs
  where
    trySequence :: [Int] -> Maybe (Int, Int)
    trySequence [] = Nothing
    trySequence (x:xs) = go x (x, x) xs
      where
        go :: Int -> (Int, Int) -> [Int] -> Maybe (Int, Int)
        go _ _ [] = Nothing
        go val result@(minVal, maxVal) (i:is)
            | val > n = Nothing
            | val == n = Just result
            | otherwise = go (val + i) (min minVal i, max maxVal i) is

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case toValues inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right values -> do
            case validateSequence 25 values of
                Right () -> putStrLn "Sequence validated."
                Left n -> do
                    putStrLn $ mconcat [ "Can't validate: ", show n ]
                    print $ uncurry (+) <$> findWeakness n values
