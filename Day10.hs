{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (sort)
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

openClose :: Map Char Char
openClose = M.fromList $ zip "([{<" ")]}>"

errorScores :: Map Char Int
errorScores = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

completionScores :: Map Char Int
completionScores = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

data LineScore = Correct | SyntaxError Int | Incomplete Int
    deriving (Show)

parseLine :: [Char] -> Either String LineScore
parseLine = go []
  where
    scoreIncomplete :: Int -> Char -> Either String Int
    scoreIncomplete score c = case M.lookup c completionScores of
        Nothing -> Left $ "Found unknown closing character: " ++ show c
        Just value -> Right $ score * 5 + value

    go :: [Char] -> [Char] -> Either String LineScore
    go [] [] = Right Correct
    go remainder [] = Incomplete <$> foldlM scoreIncomplete 0 remainder
    go (o:os) (r:rs) | o == r = go os rs
    go stack (r:rs) = case M.lookup r openClose of
        Just c -> go (c:stack) rs
        Nothing -> case M.lookup r errorScores of
            Just v -> Right (SyntaxError v)
            Nothing -> Left $ "Found unknown character: " ++ show r

findMiddle :: [a] -> Maybe a
findMiddle l = go l l
  where
    go :: [a] -> [a] -> Maybe a
    go xs [] = listToMaybe xs
    go xs [_] = listToMaybe xs
    go (_:xs) (_:_:ys) = go xs ys
    go _ _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    inputLines <- case args of
        [inputFile] -> lines <$> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case mapM parseLine inputLines of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right scores -> do
            print $ sum [n | SyntaxError n <- scores]
            print $ findMiddle (sort [n | Incomplete n <- scores])
