{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.State (State, evalState, get, modify, put)
import Data.Foldable (asum)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.Megaparsec (ParsecT, runParserT, sepBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

data Dir = D | L | U | R deriving (Eq, Show)

data Point = Point Int Int deriving (Eq, Show)

offset :: Dir -> Int -> Point -> Point
offset dir n (Point x y) = case dir of
    D -> Point x (y - n)
    L -> Point (x - n) y
    U -> Point x (y + n)
    R -> Point (x + n) y

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

origin :: Point
origin = Point 0 0

instance Ord Point where
    compare p1@(Point x1 y1) p2@(Point x2 y2)
        | EQ == distanceComparison = compare (x1, y1) (x2, y2)
        | otherwise = distanceComparison
      where
        distanceComparison = compare (manhattan origin p1) (manhattan origin p2)

type Parser = ParsecT Void Text (State Point)

direction :: Parser Dir
direction = asum
    [ D <$ char 'D'
    , L <$ char 'L'
    , U <$ char 'U'
    , R <$ char 'R'
    ]

lineSegment :: Parser (Set Point)
lineSegment = do
    Point x y <- get
    dir <- direction
    distance <- decimal
    modify (offset dir distance)
    return . S.fromList $ case dir of
        D -> map (\i -> Point x (y - i)) [1 .. distance]
        L -> map (\i -> Point (x -i) y) [1 .. distance]
        U -> map (\i -> Point x (y + i)) [1 .. distance]
        R -> map (\i -> Point (x + i) y) [1 .. distance]

line :: Parser (Set Point)
line = do
    put origin
    S.unions <$> sepBy lineSegment (char ',')

runParser :: FilePath -> Parser a -> IO a
runParser inputFile parser = do
    inputTxt <- T.readFile inputFile
    case evalState (runParserT parser inputFile inputTxt) origin of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

main :: IO ()
main = do
    args <- getArgs
    inputFile <- case args of
        [inputFile] -> return inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    (line1, line2) <- runParser inputFile $ (,) <$> line <* eol <*> line

    case S.lookupMin (S.intersection line1 line2) of
        Nothing -> putStrLn "No intersections!" >> exitFailure
        Just p -> do
            putStrLn $ "Closest intersection: " ++ show p
            putStrLn $ "Manhattan distance: " ++ show (manhattan origin p)
