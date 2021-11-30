{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

groupToSet :: Text -> Set Char
groupToSet = T.foldl' updateSet S.empty
  where
    updateSet :: Set Char -> Char -> Set Char
    updateSet set c = case c of
        '\n' -> set
        _ -> S.insert c set

processGroup :: Text -> Set Char
processGroup txt = case map groupToSet (T.lines txt) of
    [] -> S.empty
    individualAnswers -> foldl1 S.intersection individualAnswers

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.splitOn "\n\n" <$> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . sum $ map (S.size . groupToSet) inputData
    print . sum $ map (S.size . processGroup) inputData
