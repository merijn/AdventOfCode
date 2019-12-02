module Main(main) where

import Data.Foldable (foldl')
import Data.List (iterate')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import System.Exit (exitFailure)

fastSum :: [Int] -> Int
fastSum = foldl' (+) 0

toMass :: Text -> Either String [Int]
toMass = mapM toDecimal . T.lines
  where
    toDecimal :: Text -> Either String Int
    toDecimal txt = case T.decimal txt of
        Left t -> Left t
        Right (t, remainder)
            | T.null remainder -> Right t
            | otherwise -> Left "Parse error!"

computeFuel :: [Int] -> Int
computeFuel = fastSum . map toFuel
  where
    toFuel :: Int -> Int
    toFuel n = (n `div` 3) - 2

computeFuelPartTwo :: [Int] -> Int
computeFuelPartTwo = fastSum . map (fuelMassToFuel . toFuel)
  where
    toFuel :: Int -> Int
    toFuel n = (n `div` 3) - 2

    fuelMassToFuel :: Int -> Int
    fuelMassToFuel = fastSum . takeWhile (>= 0) . iterate' toFuel

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    case toMass inputData of
        Left err -> putStrLn err >> exitFailure
        Right masses -> do
            print $ computeFuel masses
            print $ computeFuelPartTwo masses
