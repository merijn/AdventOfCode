{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (replicateM, forM_)
import Data.Foldable (foldMap', foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (Endo(..))
import Data.Semigroup (stimes, Min(..), Max(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, some)
import Text.Megaparsec.Char (letterChar, eol, string)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

templateParser :: Parser String
templateParser = some letterChar <* eol

ruleParser :: Parser (Map String Char)
ruleParser = do
    pat <- replicateM 2 letterChar
    string " -> "
    c <- letterChar <* eol
    return (M.singleton pat c)

insertionRuleParser :: Parser (Map String Char)
insertionRuleParser = do
    rules <- some ruleParser
    case splatRules rules of
        Left err -> fail err
        Right result -> return result
  where
    splatRules :: [Map String Char] -> Either String (Map String Char)
    splatRules =
        M.traverseWithKey reportError . M.unionsWith combine . map (fmap Right)
      where
        combine
            :: Either String Char -> Either String Char -> Either String Char
        combine (Left vals1) (Left vals2) = Left $ vals1 ++ vals2
        combine err@Left{} _ = err
        combine _ err@Left{} = err
        combine (Right c1) (Right c2)
            | c1 == c2 = Right c1
            | otherwise = Left [c1,c2]

    reportError :: String -> Either String Char -> Either String Char
    reportError k val = case val of
        Right c -> Right c
        Left vals -> Left $ "Duplicate vals for key \'" ++ k ++ "\': " ++ vals

problemParser :: Parser (String, Map String Char)
problemParser = (,) <$> (templateParser <* eol) <*> insertionRuleParser

polymerInsertion :: Map String Char -> Int -> String -> String
polymerInsertion rules n = appEndo $ stimes n (Endo go)
  where
    go :: String -> String
    go (c1:rest@(c2:_)) = case M.lookup [c1,c2] rules of
        Nothing -> ""
        Just c -> c1:c:go rest
    go l = l

fastScore :: Map String Char -> Int -> String -> Map Char Int
fastScore rules m = scoreString True finalPairScoring
  where
    finalPairScoring :: Map String (Map Char Int)
    finalPairScoring = foldl' updateScoring pairScores subExpansions
      where
        updateScoring
            :: Map String (Map Char Int)
            -> Map String String
            -> Map String (Map Char Int)
        updateScoring scores = fmap (scoreString False scores)

    subExpansions :: [Map String String]
    subExpansions = expandN partial : replicate (full-1) (expandN baseCount)
      where
        (full, partial) = m `quotRem` baseCount

    baseCount :: Int
    baseCount = 5

    expandN :: Int -> Map String String
    expandN n = M.mapWithKey (\k _ -> polymerInsertion rules n k) rules

    pairScores :: Map String (Map Char Int)
    pairScores = M.fromListWith (+) . map (,1) . init <$> expandN baseCount

    scoreString :: Bool -> Map String (Map Char Int) -> String -> Map Char Int
    scoreString final scores = go
      where
        go :: String -> Map Char Int
        go (c1:rest@(c2:_)) = M.unionWith (+) (score [c1,c2]) (go rest)
        go [c] | final = M.singleton c 1
        go _ = M.empty

        score :: String -> Map Char Int
        score k = M.findWithDefault M.empty k scores

reportResults :: String -> Map String Char -> Int -> IO ()
reportResults template rules n = do
    putStrLn template
    print (nMax - nMin)
    forM_ (M.assocs counts) $ \(k,v) -> do
        putStrLn $ show k ++ ": " ++ show v
  where
    !counts = fastScore rules n template
    (Min nMin, Max nMax) = foldMap' (\v -> (Min v, Max v)) counts

main :: IO ()
main = do
    args <- getArgs
    (template, rules) <- case args of
        [inputFile] -> parseFile inputFile (problemParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    reportResults template rules 10
    putStrLn ""
    reportResults template rules 40
