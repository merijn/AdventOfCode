{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (forM, guard)
import Data.Char (digitToInt, isPrint, isSpace)
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Ord (Down(..))
import Data.Semigroup (ArgMin, Arg(..), Max(..), Min(..), sconcat)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, many, manyTill)
import Text.Megaparsec.Char (digitChar, eol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Position
    = Pos
      { columnPos :: {-# UNPACK #-} !Int
      , rowPos :: {-# UNPACK #-} !Int
      } deriving (Show)

instance Eq Position where
    Pos x1 y1 == Pos x2 y2 = x1 == x2 && y1 == y2

instance Ord Position where
    compare (Pos x1 y1) (Pos x2 y2) = compare x1 x2 <> compare y1 y2

rowParser :: Parser (Int -> Map Position Int)
rowParser = mconcat . zipWith mkPosMap [0..] <$> manyTill digitParser eol
  where
    digitParser :: Parser Int
    digitParser = digitToInt <$> digitChar

    mkPosMap :: Int -> v -> Int -> Map Position v
    mkPosMap x v y = M.singleton (Pos x y) v


mapParser :: Parser (Map Position Int)
mapParser = M.unions . zipWith (&) [0..] <$> many rowParser

printMap_ :: Show v => Map Position v -> IO ()
printMap_ = printMap "." show

printMap :: String -> (v -> String) -> Map Position v -> IO ()
printMap def conv coords = do
    forM [0..yMax] $ \y -> do
        forM [0..xMax] $ \x -> do
            putStr . maybe def conv $ M.lookup (Pos x y) coords
        putStrLn ""
    putStrLn ""
  where
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys coords

mapConvolution :: (v -> [v] -> Maybe v) -> Map Position v -> Map Position v
mapConvolution f = runIdentity . mapConvolutionF (\v vs -> Identity (f v vs))

mapConvolutionF
    :: Applicative f
    => (v -> [v] -> f (Maybe v)) -> Map Position v -> f (Map Position v)
mapConvolutionF convolve posMap = M.traverseMaybeWithKey updatePosition posMap
  where
    updatePosition (Pos x y) v = convolve v $ mapMaybe (`M.lookup` posMap)
        [ Pos (x-1) y
        , Pos (x+1) y
        , Pos x (y-1)
        , Pos x (y+1)
        ]

findSinks :: Map Position Int -> Map Position Int
findSinks = coerce $ mapConvolution localMinimum
  where
    localMinimum :: Min Int -> [Min Int] -> Maybe (Min Int)
    localMinimum curr vs
        | mconcat vs <= curr = Nothing
        | otherwise = Just curr

findBasins :: Map Position Int -> Map Position Char
findBasins = fmap unwrap . go . labelCells . filterBoundaries
  where
    filterBoundaries :: Map Position Int -> Map Position Int
    filterBoundaries = M.mapMaybe (\v -> v <$ guard (v /= 9))

    labels :: [Char]
    labels = filter (\c -> isPrint c && not (isSpace c)) ['a'..]

    labelCells :: Map Position Int -> Map Position (ArgMin Int Char)
    labelCells = snd . M.mapAccum (\(c:cs) v -> (cs, Min (Arg v c))) labels

    go :: Map Position (ArgMin Int Char) -> Map Position (ArgMin Int Char)
    go m = case mapConvolutionF coalesce m of
        (0, newMap) -> newMap
        (_, newMap) -> go newMap

    unwrap :: ArgMin Int Char -> Char
    unwrap (Min (Arg _ c)) = c

    coalesce
        :: ArgMin Int Char
        -> [ArgMin Int Char]
        -> (Sum Int, Maybe (ArgMin Int Char))
    coalesce v vs = (updateCount, Just newV)
      where
        newV :: ArgMin Int Char
        newV = sconcat (v :| vs)

        updateCount :: Sum Int
        updateCount = if v /= newV then 1 else 0

basinSizes :: Map Position Char -> Map Char Int
basinSizes = M.fromListWith (+) . map (,1) . M.elems

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (mapParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let riskMap = (+1) <$> findSinks inputData
        basinMap = findBasins inputData

    printMap_ inputData
    printMap_ riskMap
    printMap "." pure basinMap
    print $ sum riskMap
    print $ product . take 3 . sortOn Down . M.elems $ basinSizes basinMap
