{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<|>))
import Data.List (foldl', intercalate, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec
    (Parsec, runParser, anySingle, between, eof, sepBy1, some)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Box = Box Char deriving (Eq, Show)

data Move = Move
    { count :: Int
    , source :: Int
    , destination :: Int
    } deriving (Show)

boxParser :: Parser [Box]
boxParser = box <|> noBox
  where
    noBox = [] <$ string "   "
    box = between (char '[') (char ']') $
        (:[]) . Box <$> anySingle

boxRowParser :: Parser [[Box]]
boxRowParser = sepBy1 boxParser (char ' ') <* eol

columnIds :: Parser [Int]
columnIds = sepBy1 columnId (char ' ') <* eol
  where
    columnId :: Parser Int
    columnId = do
        char ' '
        decimal <* char ' '

boxMapParser :: Parser (Map Int [Box])
boxMapParser = do
    rows <- some boxRowParser
    ids <- columnIds
    return $ M.unionsWith (++) $ map (makeMap ids) rows
  where
    makeMap :: [Int] -> [[Box]] -> Map Int [Box]
    makeMap ids = M.unionsWith (++) . zipWith M.singleton ids

moveParser :: Parser Move
moveParser = do
    string "move "
    count <- decimal <* char ' '
    string "from "
    source <- decimal <* char ' '
    string "to "
    destination <- decimal <* eol
    return Move{..}

puzzleParser :: Parser (Map Int [Box], [Move])
puzzleParser = do
    stacks <- boxMapParser
    eol
    moves <- some moveParser
    return (stacks, moves)

renderCrates :: Map Int [Box] -> IO ()
renderCrates boxMap = mapM_ (putStrLn . intercalate " ") $ transpose columns
  where
    height :: Int
    height = maximum $ length <$> boxMap

    columns :: [[String]]
    columns = map renderColumn (M.assocs boxMap)

    renderColumn :: (Int, [Box]) -> [String]
    renderColumn (n, boxes) = reverse (colId : column)
      where
        colId = (' ' : show n ++ " ")
        column = take height $ reverse (map renderBox boxes) ++ cycle ["   "]

        renderBox :: Box -> String
        renderBox (Box c) = '[' : c :"]"

makeMove :: Map Int [Box] -> Move -> Map Int [Box]
makeMove boxMap Move{..} = M.alter insertPrefix destination tmpMap
  where
    stripStart :: Maybe [Box] -> ([Box], Maybe [Box])
    stripStart Nothing = ([], Nothing)
    stripStart (Just bs) = Just <$> splitAt count bs

    insertPrefix :: Maybe [Box] -> Maybe [Box]
    insertPrefix Nothing = Just prefix
    insertPrefix (Just bs) = Just (reverse prefix ++ bs)

    (prefix, tmpMap) = M.alterF stripStart source boxMap

topBoxes :: Map Int [Box] -> String
topBoxes = map unbox . M.elems . M.mapMaybe listToMaybe
  where
    unbox :: Box -> Char
    unbox (Box c) = c

main :: IO ()
main = do
    args <- getArgs
    (boxes, moves) <- case args of
        [inputFile] -> parseFile inputFile (puzzleParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let movedBoxes = foldl' makeMove boxes moves

    renderCrates boxes
    renderCrates movedBoxes
    putStrLn $ topBoxes movedBoxes
