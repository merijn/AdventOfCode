{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Either (partitionEithers)
import Data.Foldable (foldl', foldlM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, sepBy1)
import Text.Megaparsec.Char (char, eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

flattenMap :: MonadFail m => [IntMap v] -> m (IntMap v)
flattenMap =
    sequence . foldl' (IM.unionWithKey combine) IM.empty . map (fmap pure)
  where
    combine :: MonadFail m => Int -> m a -> m a -> m a
    combine k _ _ = fail $ "Duplicate key for: " ++ show k

data Position
    = Pos
      { columnPos :: {-# UNPACK #-} !Int
      , rowPos :: {-# UNPACK #-} !Int
      } deriving (Show)

data BingoBoard = Board
     { boardPositions :: IntMap Position
     , columnUnmarked :: Vector Int
     , rowUnmarked :: Vector Int
     } deriving (Show)

boardParser :: Parser BingoBoard
boardParser = do
    rawPositions <- mapSepParser eol rowParser [0..4] <* eol
    positions <- flattenMap rawPositions
    return Board
        { boardPositions = positions
        , columnUnmarked = VU.replicate 5 5
        , rowUnmarked = VU.replicate 5 5
        }
  where
    positionParser :: Int -> Int -> Parser (IntMap Position)
    positionParser y x = do
        val <- hspace *> decimal
        pure $ IM.singleton val (Pos x y)

    rowParser :: Int -> Parser (IntMap Position)
    rowParser y = do
        columns <- mapSepParser (char ' ') (positionParser y) [0..4]
        flattenMap columns

    mapSepParser :: Parser x -> (v -> Parser r) -> [v] -> Parser [r]
    mapSepParser _ _ [] = pure []
    mapSepParser sep mkParser (v:vs) = (:) <$> mkParser v <*> go vs
      where
        go [] = pure []
        go (a:as) = (:) <$> (sep *> mkParser a) <*> go as

problemParser :: Parser ([Int], [BingoBoard])
problemParser = do
    draws <- sepBy1 decimal (char ',') <* eol <* eol
    boards <- sepBy1 boardParser eol
    return (draws, boards)

updateBoard :: Int -> BingoBoard -> Either Int BingoBoard
updateBoard v board@Board{boardPositions,columnUnmarked,rowUnmarked} =
    case IM.updateLookupWithKey (\_ _ -> Nothing) v boardPositions of
      (Nothing, _) -> Right board
      (Just Pos{columnPos,rowPos}, newPositions) -> do
        let colVal = columnUnmarked ! columnPos
            rowVal = rowUnmarked ! rowPos

        if colVal == 1 || rowVal == 1
           then Left (v * sum (IM.keys newPositions))
           else Right Board
                { boardPositions = newPositions
                , columnUnmarked = columnUnmarked // [(columnPos, colVal - 1)]
                , rowUnmarked = rowUnmarked // [(rowPos, rowVal - 1)]
                }

firstWinner :: [BingoBoard] -> Int -> Either Int [BingoBoard]
firstWinner bs v = traverse (updateBoard v) bs

lastWinner :: [BingoBoard] -> Int -> Either Int [BingoBoard]
lastWinner bs v = case partitionEithers (map (updateBoard v) bs) of
    ([x], []) -> Left x
    (_, vs) -> Right vs

main :: IO ()
main = do
    args <- getArgs
    (draws, boards) <- case args of
        [inputFile] -> parseFile inputFile (problemParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    case foldlM firstWinner boards draws of
        Left v -> putStrLn $ "Score for first winner: " ++ show v
        Right _ -> putStrLn "No winners!"

    case foldlM lastWinner boards draws of
        Left v -> putStrLn $ "Score for last winner: " ++ show v
        Right _ -> putStrLn "No single last winner!"
