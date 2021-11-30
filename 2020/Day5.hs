module Main(main) where

import Control.Monad ((>=>))
import Data.Bifunctor (bimap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data BranchDir = Lower | Higher deriving (Show)

combine :: Applicative f => (f a, f b) -> f (a, b)
combine (x, y) = (,) <$> x <*> y

binarySearch :: Int -> [BranchDir] -> Maybe Int
binarySearch val = go 0 val
  where
    go :: Int -> Int -> [BranchDir] -> Maybe Int
    go lo hi []
        | lo == hi = Just hi
        | otherwise = Nothing

    go lo hi _ | lo == hi = Nothing

    go lo hi (b:bs) = case b of
        Lower -> go lo (hi - diff) bs
        Higher -> go (lo + diff) hi bs
      where
        diff = (hi - lo + 1) `div` 2

lineToIndicators :: String -> Maybe ([BranchDir], [BranchDir])
lineToIndicators = combine . parseDirs . splitLine
  where
    splitLine :: String -> (String, String)
    splitLine = span (\c -> c == 'F' || c == 'B')

    parseDirs :: (String, String) -> (Maybe [BranchDir], Maybe [BranchDir])
    parseDirs = bimap (mapM rowToBranchDir) (mapM seatToBranchDir)

    rowToBranchDir :: Char -> Maybe BranchDir
    rowToBranchDir c = case c of
        'F' -> Just Lower
        'B' -> Just Higher
        _ -> Nothing

    seatToBranchDir :: Char -> Maybe BranchDir
    seatToBranchDir c = case c of
        'L' -> Just Lower
        'R' -> Just Higher
        _ -> Nothing

indicatorToSeatNumber :: ([BranchDir], [BranchDir]) -> Maybe Int
indicatorToSeatNumber (rowDir, seatDir)= do
    rowInd <- binarySearch 127 rowDir
    seatInd <- binarySearch 7 seatDir
    return $ rowInd * 8 + seatInd

lookupMissing :: [Int] -> Maybe IntSet
lookupMissing is = do
    (minVal, remainder) <- IS.minView idxSet
    (maxVal, rest) <- IS.maxView remainder
    return $ IS.difference (IS.fromList [minVal .. maxVal]) rest
  where
    idxSet = IS.fromList is

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> lines <$> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case mapM (lineToIndicators >=> indicatorToSeatNumber) inputData of
        Nothing -> hPutStrLn stderr "Parse error!" >> exitFailure
        Just l -> do
            print $ maximum l
            print $ lookupMissing l
