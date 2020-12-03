{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

descentSlope :: [Text] -> (Int, Int) -> Int
descentSlope [] _ = 0
descentSlope entireSlope@(slope:_) (dx, dy) =
    snd $ foldl' checkTree (0, 0) filteredSlope
   where
     width :: Int
     width = T.length slope

     filteredSlope :: [Text]
     filteredSlope = map snd . filter fst $ zip filterPred entireSlope
       where
         filterPred = cycle $ True : replicate (dy - 1) False

     checkTree :: (Int, Int) -> Text -> (Int, Int)
     checkTree (!x, !count) nextLine = (x + dx, newCount)
        where
          newCount | nextLine `T.index` (x `mod` width) == '#' = count+1
                   | otherwise = count

main :: IO ()
main = do
    args <- getArgs
    inputLines <- case args of
        [inputFile] -> T.lines <$> T.readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print $ descentSlope inputLines (3, 1)
    print . product . map (descentSlope inputLines) $
        [(1,1),(3,1),(5,1),(7,1),(1,2)]
