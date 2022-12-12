module Main where

import Control.Monad (forM_)
import Data.List (transpose)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

interleaveChunks :: Int -> Text -> [Text]
interleaveChunks n input = mconcat . transpose $ map rechunk [0..n-1]
  where
    rechunk :: Int -> [Text]
    rechunk k = T.chunksOf n . T.drop k $ input

noDoubles :: Text -> Bool
noDoubles = (==4) . S.size . T.foldr' S.insert S.empty

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> TIO.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure


    let chunkSize = 4
        chunks = interleaveChunks chunkSize inputData
        result = dropWhile (not . noDoubles . snd) $ zip [0..] chunks

    case result of
        [] -> putStrLn "No marker found!" >> exitFailure
        ((n,_):_) -> putStrLn $ "Marker found at: " ++ show (n+4)

