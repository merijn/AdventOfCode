module Main(main) where

import Data.Bifunctor (bimap, first)
import Data.Foldable (foldMap')
import Data.List ((\\), iterate')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Semigroup (Max(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Seat = Empty | Taken deriving Show

seatIndices :: [String] -> Map (Int, Int) Seat
seatIndices =
    M.fromList . concat . zipWith rowIndices [0..] . map columnIndices
  where
    rowIndices :: Int -> [(Int, Seat)] -> [((Int, Int), Seat)]
    rowIndices y = map addRow
      where
        addRow :: (Int, Seat) -> ((Int, Int), Seat)
        addRow (x, seat) = ((x,y), seat)

    columnIndices :: String -> [(Int, Seat)]
    columnIndices = mapMaybe toSeat . zip [0..]
      where
        toSeat :: (Int, Char) -> Maybe (Int, Seat)
        toSeat (n, c) = case c of
            '#' -> Just (n, Taken)
            'L' -> Just (n, Empty)
            _ -> Nothing

renderSeats :: Map (Int, Int) Seat -> String
renderSeats seats = unlines $ map makeRow [0..yMax]
  where
    (xMax, yMax) = case foldMap (Just . bimap Max Max) (M.keys seats) of
        Nothing -> (0, 0)
        Just (Max x, Max y) -> (x, y)

    makeRow :: Int -> String
    makeRow n = map (\x -> lookupSeat x n) [0..xMax]

    lookupSeat :: Int -> Int -> Char
    lookupSeat x y = maybe '.' seatToChar $ M.lookup (x, y) seats

    seatToChar :: Seat -> Char
    seatToChar s = case s of
        Empty -> 'L'
        Taken -> '#'

updateSeats :: Map (Int, Int) Seat -> (Int, Map (Int, Int) Seat)
updateSeats seats = first getSum $ M.traverseWithKey updateSeat seats
  where
    updateSeat :: (Int, Int) -> Seat -> (Sum Int, Seat)
    updateSeat idx Empty | takenNeighbours idx == 0 = (1, Taken)
    updateSeat idx Taken | takenNeighbours idx >= 4 = (1, Empty)
    updateSeat _ status = (0, status)

    neighbours :: (Int, Int) -> [(Int, Int)]
    neighbours idx@(x, y) = ((,) <$> [x-1..x+1] <*> [y-1..y+1]) \\ [idx]

    takenNeighbours :: (Int, Int) -> Int
    takenNeighbours = getSum . foldMap' taken . neighbours
      where
        taken :: (Int, Int) -> Sum Int
        taken idx = case M.lookup idx seats of
            Nothing -> 0
            Just Empty -> 0
            Just Taken -> 1

seatEvolution :: Map (Int, Int) Seat -> [Map (Int, Int) Seat]
seatEvolution seats = original : map snd (takeWhile ((>0) . fst) evolution)
  where
    ((_, original):evolution) = iterate' (updateSeats . snd) (0, seats)

countSeats :: Foldable f => f Seat -> Int
countSeats = getSum . foldMap' seatToCount
  where
    seatToCount :: Seat -> Sum Int
    seatToCount Empty = 0
    seatToCount Taken = 1

main :: IO ()
main = do
    args <- getArgs
    inputSeats <- case args of
        [inputFile] -> seatIndices . lines <$> readFile inputFile
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    mapM_ (putStrLn . renderSeats) $ seatEvolution inputSeats
    print . countSeats . last $ seatEvolution inputSeats
