module Main where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

priorities :: Map Char Int
priorities = M.fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([], [])
splitInHalf l = go id l l
  where
    go :: ([b] -> [b]) -> [b] -> [b] -> ([b], [b])
    go f xs [] = (f [], xs)
    go f xs [_] = (f [], xs)
    go f (x:xs) (_:_:ys) = go (f . (x:)) xs ys
    go _ [] (_:_) = error "can't happen"

parseBackpack :: String -> (Set Char, Set Char)
parseBackpack = bimap S.fromList S.fromList . splitInHalf

scoreBackpack :: (Set Char, Set Char) -> Int
scoreBackpack (s1, s2) = sum . map scorePrio . S.elems $ S.intersection s1 s2

scorePrio :: Char -> Int
scorePrio c = M.findWithDefault 0 c priorities

parseGroups :: [String] -> [(Set Char, Set Char, Set Char)]
parseGroups [] = []
parseGroups (s1:s2:s3:ss) =
    (S.fromList s1, S.fromList s2, S.fromList s3) : parseGroups ss
parseGroups _ = []

scoreGroup :: (Set Char, Set Char, Set Char)-> Int
scoreGroup (s1, s2, s3) = sum . map scorePrio $ S.elems badges
  where
    badges = s1 `S.intersection` s2 `S.intersection` s3

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> lines <$> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let backpackScores = map (scoreBackpack . parseBackpack) inputData
        groupScores = map scoreGroup $ parseGroups inputData

    putStrLn "Puzzle #1:"
    print $ sum backpackScores
    putStrLn "Puzzle #2:"
    print $ sum groupScores
