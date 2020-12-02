{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, manyTill)
import Text.Megaparsec.Char (char, letterChar, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Password = Password
    { minCount :: Int
    , maxCount :: Int
    , letter :: Char
    , password :: String
    } deriving Show

parsePassword :: Parser Password
parsePassword = do
    minCount <- decimal <* char '-'
    maxCount <- decimal <* char ' '
    letter <- letterChar <* string ": "
    password <- manyTill letterChar eol
    return Password{..}

isValidPassword1 :: Password -> Bool
isValidPassword1 Password{..} =
    minCount <= letterCount && letterCount <= maxCount
  where
    letterCount :: Int
    letterCount = getSum $ M.findWithDefault 0 letter characterCounts

    characterCounts :: Map Char (Sum Int)
    characterCounts = M.fromListWith mappend . map (,1) $ password

isValidPassword2 :: Password -> Bool
isValidPassword2 Password{..} = 1 == length [()
    | (i, c) <- indexedPassword
    , i == minCount || i == maxCount
    , c == letter
    ]
  where
    indexedPassword :: [(Int, Char)]
    indexedPassword = zip [1..] password

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (manyTill parsePassword eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . length $ filter isValidPassword1 inputData
    print . length $ filter isValidPassword2 inputData
