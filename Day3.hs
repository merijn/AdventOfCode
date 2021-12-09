{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, manyTill)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

diagnosticsParser :: Parser (Set (Vector Int))
diagnosticsParser = do
    (n, initVec) <- firstVector
    rest <- manyTill (fixedVector n) eof
    return $ S.fromList (initVec:rest)
  where
    digit :: Parser Int
    digit = (0 <$ char '0') <|> (1 <$ char '1')

    firstVector :: Parser (Int, Vector Int)
    firstVector = do
        digits <- VU.fromList <$> manyTill digit eol
        return (VU.length digits, digits)

    fixedVector :: Int -> Parser (Vector Int)
    fixedVector n = VU.replicateM n digit <* eol

computeParity :: Set (Vector Int) -> (Int, Vector Int)
computeParity diagnostics = case S.minView diagnostics of
    Nothing -> (0, VU.empty)
    Just (v, vs) -> foldl' updateCount (0, v) vs
  where
    updateCount :: (Int, Vector Int) -> Vector Int -> (Int, Vector Int)
    updateCount (!n, !sums) vec = (n + 1, VU.zipWith (+) sums vec)

computeGammaEpsilon :: Set (Vector Int) -> (Int, Int)
computeGammaEpsilon diagnostics = (vecToVal (> k) vec, vecToVal (<= k) vec)
  where
    (k, vec) = first (`div` 2) $ computeParity diagnostics

vecToVal :: (Int -> Bool) -> Vector Int -> Int
vecToVal p = VU.ifoldl' updateSum 0 . VU.reverse
  where
    updateSum :: Int -> Int -> Int -> Int
    updateSum val idx x
        | p x = val + 2^idx
        | otherwise = val

computeOxygenCO2 :: Set (Vector Int) -> (Int, Int)
computeOxygenCO2 diagnostics = (filterDiagnostics (==), filterDiagnostics (/=))
  where
    bitParity :: Set (Vector Int) -> Int -> Int
    bitParity vecs idx
        | count >= (S.size vecs + 1) `div` 2 = 1
        | otherwise = 0
      where
        count = foldl' (\n v -> n + (v ! idx)) 0 vecs

    filterDiagnostics :: (Int -> Int -> Bool) -> Int
    filterDiagnostics cmp = go 0 diagnostics
      where
        go :: Int -> Set (Vector Int) -> Int
        go idx vecs
            | S.size vecs > 1 = go (idx + 1) (S.filter filterPred vecs)
            | otherwise = case S.minView vecs of
                Nothing -> 0
                Just (v, _) -> vecToVal (==1) v
          where
            parity :: Int
            parity = bitParity vecs idx

            filterPred :: Vector Int -> Bool
            filterPred vec = vec ! idx `cmp` parity

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (diagnosticsParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let (gamma, epsilon) = computeGammaEpsilon inputData
        (oxygen, co2) = computeOxygenCO2 inputData

    putStr $ unlines
        [ "Gamma: " ++ show gamma
        , "Epsilon: " ++ show epsilon
        , "Result: " ++ show (gamma * epsilon)
        , "Oxygen generator: " ++ show oxygen
        , "CO2 scrubber: " ++ show co2
        , "Result: " ++ show (oxygen * co2)
        ]
