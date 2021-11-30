{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

bagContents :: Parser [(Int, Text)]
bagContents = (bagCounts <|> noBags) <* Mega.char '.'
  where
    noBags :: Parser [a]
    noBags = [] <$ Mega.string "no other bags"

    bagCounts :: Parser [(Int, Text)]
    bagCounts = Mega.sepBy1 bagCount (Mega.string ", ")

bagCount :: Parser (Int, Text)
bagCount = do
    n <- Mega.decimal <* Mega.char ' '
    bagType <- Mega.someTill Mega.anySingle bagTxt
    return (n, T.pack bagType)
  where
    bagTxt :: Parser ()
    bagTxt = void $ Mega.string " bag" *> Mega.optional (Mega.char 's')

bagRule :: Parser (Map Text [(Int, Text)])
bagRule = do
    bagType <- Mega.someTill Mega.anySingle (Mega.string " bags contain ")
    contents <- bagContents
    return $ M.singleton (T.pack bagType) contents

allRules :: Parser (Map Text [(Int, Text)])
allRules = do
    rules <- Mega.sepEndBy1 bagRule Mega.eol <* Mega.eof
    case sequence (M.unionsWith checkKeys (map (fmap Right) rules)) of
        Left err -> fail err
        Right results -> return results
  where
    checkKeys
        :: Either String [(Int, Text)]
        -> Either String [(Int, Text)]
        -> Either String [(Int, Text)]
    checkKeys _ _ = Left $ "Duplicate keys"

containsGolden :: Map Text [(Int, Text)] -> Text -> Bool
containsGolden bagRules = go
  where
    go :: Text -> Bool
    go name = case M.lookup name bagRules of
        Nothing -> False
        Just [] -> False
        Just bags -> any checkGolden bags

    checkGolden :: (Int, Text) -> Bool
    checkGolden (_, name) = case name of
        "shiny gold" -> True
        _ -> go name

computeBagCount :: Map Text [(Int, Text)] -> Text -> Int
computeBagCount bagRules = go
  where
    go :: Text -> Int
    go name = case M.lookup name bagRules of
        Nothing -> 0
        Just bags -> sum $ map checkCounts bags

    checkCounts :: (Int, Text) -> Int
    checkCounts (n, name) = n + (n * go name)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile allRules
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . length . filter (containsGolden inputData) $ M.keys inputData
    print $ computeBagCount inputData "shiny gold"
