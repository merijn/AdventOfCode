module Main where

import Control.Monad (forM_)
import Data.Bifunctor (first, second)
import Data.Char (digitToInt)
import Data.Foldable (foldMap')
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Sum(..))
import Data.Semigroup (Max(..))
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
    (Max xMax, Max yMax) =
        foldMap' (\(Pos x y) -> (Max x, Max y)) $ M.keys coords

mapConvolution :: (v -> [v] -> r) -> Map Position v -> Map Position r
mapConvolution f = runIdentity . mapConvolutionF (\v vs -> Identity (f v vs))

mapConvolutionF
    :: Applicative f
    => (v -> [v] -> f r) -> Map Position v -> f (Map Position r)
mapConvolutionF convolve posMap = M.traverseWithKey updatePosition posMap
  where
    updatePosition pos@(Pos x y) v =
        convolve v $ mapMaybe (`M.lookup` posMap) nbrs
      where
        nbrs = filter (/= pos) $ Pos <$> [x-1 .. x+1] <*> [y-1 .. y +1]

fullStep :: Map Position Int -> (Sum Int, Map Position Int)
fullStep = second updateEnergy . go . incEnergy
  where
    incEnergy :: Map Position Int -> Map Position (Bool, Int)
    incEnergy = M.map (\n -> (False, n+1))

    updateEnergy :: Map Position (Bool, Int) -> Map Position Int
    updateEnergy = M.map (\(_, n) -> if n > 9 then 0 else n)

    triggerFlashes
        :: Map Position (Bool, Int) -> (Sum Int, Map Position (Bool, Int))
    triggerFlashes = mapConvolutionF absorbAndFlash
      where
        absorbAndFlash
            :: (Bool, Int) -> [(Bool, Int)] -> (Sum Int, (Bool, Int))
        absorbAndFlash state@(True, _) _ = (0, state)
        absorbAndFlash (False, myEnergy) nbrs
            | myEnergy > 9 = (1, (True, myEnergy))
            | otherwise = (0, (False, newEnergy))
          where
            newEnergy = myEnergy + length [() | (False, n) <- nbrs, n > 9]

    go :: Map Position (Bool, Int) -> (Sum Int, Map Position (Bool, Int))
    go posMap
        | updateCount > 0 = first (+ updateCount) $ go newMap
        | otherwise = (0, newMap)
        where
          (updateCount, newMap) = triggerFlashes posMap

energySteps :: Map Position Int -> [(Sum Int, Map Position Int)]
energySteps = unfoldr mkEnergyStep
  where
    mkEnergyStep
        :: Map Position Int
        -> Maybe ((Sum Int, Map Position Int), Map Position Int)
    mkEnergyStep oldMap = Just ((updateCount, newMap),  newMap)
      where
        (updateCount, newMap) = fullStep oldMap

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (mapParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let energyLevels :: [(Sum Int, Map Position Int)]
        energyLevels = energySteps inputData

        stepFlashes :: [(Int, Sum Int)]
        stepFlashes = zip [1..] (map fst energyLevels)

    print . getSum . foldMap' fst $ take 100 energyLevels
    print . fmap fst . listToMaybe . dropWhile ((/=100) . snd) $ stepFlashes
