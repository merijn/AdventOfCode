{-# LANGUAGE TupleSections #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

openClose :: Map Char Char
openClose = M.fromList $ zip "([{<" ")]}>"

errorScores :: Map Char Int
errorScores = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

parseLine :: [Char] -> Either String Int
parseLine = go []
  where
    go :: [Char] -> [Char] -> Either String Int
    go _ [] = Right 0
    go (o:os) (r:rs) | o == r = go os rs
    go stack (r:rs) = case M.lookup r openClose of
        Just c -> go (c:stack) rs
        Nothing -> case M.lookup r errorScores of
            Just v -> Right v
            Nothing -> Left $ "Found unknown character: " ++ show r

main :: IO ()
main = do
    args <- getArgs
    inputLines <- case args of
        [inputFile] -> lines <$> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print $ sum <$> mapM parseLine inputLines
    return ()
