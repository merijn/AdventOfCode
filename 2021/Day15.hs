{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Foldable (foldMap', foldl')
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Semigroup (Max(..))
import Data.Set (Set)
import qualified Data.Set as S
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
    forM_ [0..yMax] $ \y -> do
        forM_ [0..xMax] $ \x -> do
            putStr . maybe def conv $ M.lookup (Pos x y) coords
        putStrLn ""
    putStrLn ""
  where
    (xMax, yMax) = mapRange coords

mapRange :: Map Position v -> (Int, Int)
mapRange coords = (xMax, yMax)
  where
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys coords

data Estimate = Estimate
    { minCost :: Int
    , currPos :: Position
    , path :: [Position]
    } deriving (Eq, Show)

instance Ord Estimate where
    compare = comparing minCost <> comparing currPos <> comparing path

expandMap :: (Int, Int) -> Map Position Int -> Map Position Int
expandMap (xFactor, yFactor) posMap = foldMap' expandPoint $ M.assocs posMap
  where
    (xMax, yMax) = bimap (+1) (+1) $ mapRange posMap

    translationRange :: [(Int, Int)]
    translationRange = (,) <$> [0..xFactor-1] <*> [0..yFactor-1]

    expandPoint :: (Position, Int) -> Map Position Int
    expandPoint = foldMap' translatePosition translationRange

    translatePosition :: (Int, Int) -> (Position, Int) -> Map Position Int
    translatePosition (translateX, translateY) (Pos x y, v) =
        M.singleton newPos roundedVal
      where
        newPos :: Position
        newPos = Pos (translateX * xMax + x) (translateY * yMax + y)

        newValue :: Int
        newValue = v + translateX + translateY

        roundedVal :: Int
        roundedVal | newValue <= 9 = newValue
                   | otherwise = (newValue `mod` 10) + 1

findShortestPath :: Map Position Int -> Maybe (Int, [Position])
findShortestPath posMap =
    go (S.singleton startEstimate) (M.singleton startPos startEstimate)
  where
    startPos :: Position
    startPos = Pos 0 0

    endPos :: Position
    endPos = uncurry Pos $ mapRange posMap

    startEstimate :: Estimate
    startEstimate = Estimate 0 startPos [startPos]

    checkRouteFound :: [Estimate] -> Maybe (Int, [Position])
    checkRouteFound ests = estimateToResult <$> findEndPos ests
      where
        findEndPos :: [Estimate] -> Maybe Estimate
        findEndPos = listToMaybe . filter ((== endPos) . currPos)

        estimateToResult :: Estimate -> (Int, [Position])
        estimateToResult est = (minCost est, path est)

    go :: Set Estimate -> Map Position Estimate -> Maybe (Int, [Position])
    go frontier bestEsts = S.minView frontier >>= nextStep
      where
        nextStep :: (Estimate, Set Estimate) -> Maybe (Int, [Position])
        nextStep (est, oldFrontier) =
            checkRouteFound newEsts <|> go newFrontier newBestEsts
          where
            newRouteEsts = mapMaybe (nbrEstimates est) $ nbrs est
            (newBestEsts, newEsts) =
                foldl' updateEstimate (bestEsts, []) newRouteEsts
            newFrontier = S.fromList newEsts <> oldFrontier

    updateEstimate
        :: (Map Position Estimate, [Estimate])
        -> Estimate
        -> (Map Position Estimate, [Estimate])
    updateEstimate (ests, r) est = case M.lookup (currPos est) ests of
        Just oldEst | oldEst < est -> (ests, r)
        _ -> (M.insert (currPos est) est ests, est:r)

    nbrEstimates :: Estimate -> Position -> Maybe Estimate
    nbrEstimates Estimate{..} p = case M.lookup p posMap of
        Nothing -> Nothing
        Just n -> Just Estimate
            { minCost = minCost + n, currPos = p, path = p:path }

    nbrs :: Estimate -> [Position]
    nbrs Estimate{currPos = Pos x y} =
        [Pos (x-1) y, Pos (x+1) y, Pos x (y-1), Pos x (y+1)]

main :: IO ()
main = do
    args <- getArgs
    smallMap <- case args of
        [inputFile] -> parseFile inputFile (mapParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let expandedMap = expandMap (5, 5) smallMap

    printMap_ smallMap
    case findShortestPath smallMap of
        Nothing -> hPutStrLn stderr "No path!" >> exitFailure
        Just (cost, path) -> do
            printMap_ $ M.restrictKeys smallMap (S.fromList path)
            print cost

    case findShortestPath expandedMap of
        Nothing -> hPutStrLn stderr "No path!" >> exitFailure
        Just (cost, path) -> do
            printMap_ $ M.restrictKeys expandedMap (S.fromList path)
            print cost
