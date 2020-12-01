module Main(main) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Monoid (Ap(Ap,getAp))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

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

toValues :: Text -> Either String IntSet
toValues = getAp . foldMap (Ap . toSingleton) . T.lines
  where
    toSingleton :: Text -> Either String IntSet
    toSingleton txt = case T.decimal txt of
        Left t -> Left t
        Right (t, remainder)
            | T.null remainder -> Right (IS.singleton t)
            | otherwise -> Left "Parse error!"

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case toValues inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right values -> print $ uncurry (*) <$> solveSum 2020 values
