{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Either (rights)
import Data.Foldable (asum)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector, (!?), (!), (//))
import qualified Data.Vector as V
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, try)
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

data Instr = Acc Int | Jump Int | Nop Int deriving Show

instrParser :: Parser (Int -> Instr)
instrParser = asum
    [ Acc <$ try (Mega.string "acc")
    , Jump <$ try (Mega.string "jmp")
    , Nop <$ try (Mega.string "nop")
    ]

signedVal :: Parser Int
signedVal = Mega.signed (return ()) Mega.decimal

instruction :: Parser Instr
instruction = instrParser <* Mega.hspace1 <*> signedVal

instructions :: Parser (Vector Instr)
instructions = V.fromList <$> Mega.sepEndBy1 instruction Mega.eol

interpretUntilLoop :: Vector Instr -> Maybe Int
interpretUntilLoop vec = go 0 0 IS.empty
  where
    go :: Int -> Int -> IntSet -> Maybe Int
    go idx val visited
        | idx `IS.member` visited = Just val
        | otherwise = vec !? idx >>= \case
            Nop _ -> go (idx + 1) val newVisited
            Acc n -> go (idx + 1) (val + n) newVisited
            Jump n -> go (idx + n) val newVisited
        where
          newVisited = IS.insert idx visited

interpret :: Vector Instr -> Either String Int
interpret vec = go 0 0 IS.empty
  where
    endInstr :: Int
    endInstr = V.length vec

    go :: Int -> Int -> IntSet -> Either String Int
    go idx val _ | idx == endInstr = Right val
    go idx _ visited | idx `IS.member` visited =
        Left $ "Encountered loop on instruction: " ++ show idx

    go idx _ _ | idx < 0 || idx >= endInstr =
        Left $ "Encountered out of bounds instruction: " ++ show idx

    go idx val visited = case vec ! idx of
            Nop _ -> go (idx + 1) val newVisited
            Acc n -> go (idx + 1) (val + n) newVisited
            Jump n -> go (idx + n) val newVisited
        where
          newVisited = IS.insert idx visited

possibleInstructions :: Vector Instr -> [Vector Instr]
possibleInstructions vec = vec : mapMaybe instrAlternatives [0..finalIdx]
  where
    finalIdx :: Int
    finalIdx = V.length vec - 1

    instrAlternatives :: Int -> Maybe (Vector Instr)
    instrAlternatives idx = case vec ! idx of
        Nop n -> Just $ vec // [(idx, Jump n)]
        Jump n -> Just $ vec // [(idx, Nop n)]
        Acc _ -> Nothing

findSuccess :: Vector Instr -> Maybe Int
findSuccess = listToMaybe . rights . map interpret . possibleInstructions

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (instructions <* Mega.eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print $ interpretUntilLoop inputData
    mapM_ print $ findSuccess inputData
