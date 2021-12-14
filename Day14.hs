{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (replicateM, forM_)
import Data.Foldable (foldMap')
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

fastInsertionRules
    :: Map String Char -> Int -> Map String String -> Map String String
fastInsertionRules baseRules n = fmap insert
  where
    insert :: String -> String
    insert = polymerInsertion baseRules n

applyRules :: forall v . (v -> v -> v) -> v -> Map String v -> String -> v
applyRules combine def rules = go
  where
    go :: String -> v
    go (c1:rest@(c2:_)) = case M.lookup [c1,c2] rules of
        Nothing -> def
        Just c -> c `combine` go rest
    go l = case M.lookup l rules of
        Nothing -> def
        Just v -> v

tidyRules :: Map String String -> Map String String
tidyRules rules = M.union singleChars (M.map init rules)
  where
    allChars :: String
    allChars = mconcat $ M.keys rules

    singleChars :: Map String String
    singleChars = foldMap (\c -> M.singleton [c] [c]) allChars

fastInsert :: Map String String -> String -> String
fastInsert = applyRules (++) [] . tidyRules

fastCount :: Map String String -> String -> Map Char Int
fastCount rules = applyRules (M.unionWith (+)) M.empty scoreRules
  where
    scoreRules :: Map String (Map Char Int)
    scoreRules = countMap <$> tidyRules rules

countMap :: String -> Map Char Int
countMap = M.fromListWith (+) . map (,1)

main :: IO ()
main = do
    args <- getArgs
    (template, rules) <- case args of
        [inputFile] -> parseFile inputFile (problemParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let basePairs = M.mapWithKey (\k _ -> k) rules
        !fastRules = fastInsertionRules rules 20 basePairs
        !counts = fastCount fastRules . fastInsert fastRules $ template
        (Min nMin, Max nMax) = foldMap' (\v -> (Min v, Max v)) counts

    putStrLn template
    print (nMax - nMin)
    forM_ (M.assocs counts) $ \(k,v) -> do
        putStrLn $ show k ++ ": " ++ show v
