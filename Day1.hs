module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

toValues :: Text -> Either String [[Int]]
toValues = fmap groupMaybes . traverse toNum . T.lines
  where
    toNum :: Text -> Either String (Maybe Int)
    toNum txt
        | T.null txt = Right Nothing
        | otherwise = case T.decimal txt of
            Left t -> Left t
            Right (i, remainder)
                | T.null remainder -> Right (Just i)
                | otherwise -> Left "Parse error!"

    groupMaybes :: [Maybe Int] -> [[Int]]
    groupMaybes = go id
      where
        go :: ([Int] -> [Int]) -> [Maybe Int] -> [[Int]]
        go f [] = [f []]
        go f (Nothing:xs) = f [] : go id xs
        go f (Just i:xs) = go (f . (i:)) xs

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case toValues inputData of
        Left err -> putStrLn $ "Error reading input: " <> err
        Right v -> print $ maximum (map sum v)
