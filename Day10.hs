{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Control.Monad (guard)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(Ap,getAp), Sum(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

toValues :: Text -> Either String IntSet
toValues = getAp . foldMap (Ap . toSingleton) . T.lines
  where
    toSingleton :: Text -> Either String IntSet
    toSingleton txt = case T.decimal txt of
        Left t -> Left t
        Right (t, remainder)
            | T.null remainder -> Right (IS.singleton t)
            | otherwise -> Left "Parse error!"

joltDiffs :: IntSet -> IntMap (Sum Int)
joltDiffs inputs =
    IM.fromListWith mappend . map (,1) . unfoldr joltDiff $ (0, extendedInput)
  where
    joltDiff :: (Int, IntSet) -> Maybe (Int, (Int, IntSet))
    joltDiff (old, values) = do
        result@(new, _) <- IS.minView values
        guard (new - old <= 3)
        return (new - old, result)

    extendedInput :: IntSet
    extendedInput = case IS.maxView inputs of
        Nothing -> IS.empty
        Just (maxVal, _) -> IS.insert (maxVal + 3) inputs

splitSequence :: IntSet -> [IntSet]
splitSequence = map IS.fromList . split . IS.toAscList
  where
    split :: [Int] -> [[Int]]
    split xs = go [] xs

    go :: [Int] -> [Int] -> [[Int]]
    go xs [] = [xs]
    go [] (x:xs) = go [x] xs
    go ys@(y:_) (x:xs)
        | x - y >= 3 = ys : go [x] xs
        | otherwise = go (x:ys) xs

numSubsetPaths :: IntSet -> Int
numSubsetPaths inputs = fromMaybe 1 $ do
    (minVal, remainder) <- IS.minView inputs
    (maxVal, values) <- IS.maxView remainder
    return $ go 0 (maxVal, minVal) values
  where
    go :: Int -> (Int, Int) -> IntSet -> Int
    go !val (target, current) remainder = case IS.minView remainder of
        Nothing -> if target - current <= 3 then 1 + val else val
        Just (s, r)
            | s - current > 3 -> val
            | otherwise -> go (go val (target, current) r) (target, s) r

numPaths :: IntSet -> Int
numPaths = product . map numSubsetPaths . splitSequence

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case IS.insert 0 <$> toValues inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right values -> do
            print $ joltDiffs values
            print $ numPaths values
