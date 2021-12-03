{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
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

computeGammaEpsilon :: Set (Vector Int) -> (Int, Int)
computeGammaEpsilon diagnostics = case S.minView diagnostics of
    Nothing -> (0, 0)
    Just (v, vs) -> let
        (k, countVec) = first (`div` 2) $ foldl' updateCount (0, v) vs
      in (vecToVal (> k) countVec, vecToVal (<= k) countVec)
  where
    updateCount :: (Int, Vector Int) -> Vector Int -> (Int, Vector Int)
    updateCount (!n, !sums) vec = (n + 1, VU.zipWith (+) sums vec)

    vecToVal :: (Int -> Bool) -> Vector Int -> Int
    vecToVal p = VU.ifoldl' updateSum 0 . VU.reverse
      where
        updateSum :: Int -> Int -> Int -> Int
        updateSum val idx x
            | p x = val + 2^idx
            | otherwise = val

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (diagnosticsParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let (gamma, epsilon) = computeGammaEpsilon inputData
    putStr $ unlines
        [ "Gamma: " ++ show gamma
        , "Epsilon: " ++ show epsilon
        , "Result: " ++ show (gamma * epsilon)
        ]
