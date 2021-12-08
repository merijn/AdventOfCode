module Main where

import Control.Monad ((>=>), foldM, replicateM)
import Data.Foldable (asum, foldl', toList)
import Data.Traversable (mapAccumR)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec (Parsec, runParser, eof, many, some)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data Segment = A | B | C | D | E | F | G
    deriving (Eq, Ord, Show)

newtype Digit = Digit { digitSegments :: Set Segment }
    deriving (Eq, Ord, Show)

segmentParser :: Parser Segment
segmentParser = asum
    [ A <$ char 'a', B <$ char 'b', C <$ char 'c', D <$ char 'd'
    , E <$ char 'e', F <$ char 'f', G <$ char 'g'
    ]

digitParser :: Parser Digit
digitParser = Digit . S.fromList <$> some segmentParser

displayParser :: Parser ([Digit], [Digit])
displayParser = do
    patterns <- replicateM 10 (digitParser <* char ' ')
    char '|'
    display <- replicateM 4 (char ' ' *> digitParser) <* eol
    return (patterns, display)

digitIntMap :: Map Digit Int
digitIntMap =  M.fromList pairs
  where
    pairs = map (\(v, k) -> (Digit (S.fromList k), v))
        [ (0, [A, B, C, E, F, G])
        , (1, [C, F])
        , (2, [A, C, D, E, G])
        , (3, [A, C, D, F, G])
        , (4, [B, C, D, F])
        , (5, [A, B, D, F, G])
        , (6, [A, B, D, E, F, G])
        , (7, [A, C, F])
        , (8, [A, B, C, D, E, F, G])
        , (9, [A, B, C, D, F, G])
        ]

digitToInt :: Digit -> Either String Int
digitToInt d = case M.lookup d digitIntMap of
    Nothing -> Left $ "Non-existent digit: " ++ show d
    Just v -> Right v

digitsBySegmentCount :: Foldable f => f Digit -> Map Int (Set Digit)
digitsBySegmentCount = M.fromListWith S.union . map toPair . toList
  where
    toPair :: Digit -> (Int, Set Digit)
    toPair d@(Digit s) = (S.size s, S.singleton d)

digitSetToSegmentSets
    :: Set Digit -> (Set Segment, Set Segment)
digitSetToSegmentSets digits = case S.minView digits of
    Nothing -> (S.empty, S.empty)
    Just (Digit s, ds) -> foldl' update (s, s) ds
  where
    update :: (Set Segment, Set Segment) -> Digit -> (Set Segment, Set Segment)
    update (s1, s2) (Digit s) = (s1 `S.intersection` s, s2 `symmDiff` s)

    symmDiff :: Ord v => Set v -> Set v -> Set v
    symmDiff s1 s2 = (s1 `S.union` s2) `S.difference` (s1 `S.intersection` s2)

expandToMap :: (Foldable f, Ord k) => v -> f k -> Map k v
expandToMap v = M.fromList . map (\k -> (k, v)) . toList

simplifyConstraints
    :: (Ord k, Ord v) => Map k (Set v) -> Either String (Map k v)
simplifyConstraints = go
  where
    go :: (Ord k, Ord v) => Map k (Set v) -> Either String (Map k v)
    go m = do
        uniq <- foldM uniqueConstraints S.empty m
        let (remaining, n) = mapAccumR (reduceConstraints uniq) 0 m
        if remaining > 0 then go n else traverse unsetify n

    uniqueConstraints :: Ord v => Set v -> Set v -> Either String (Set v)
    uniqueConstraints uniqs constraint
        | S.size constraint /= 1 = Right uniqs
        | otherwise = case S.minView constraint of
            Nothing -> Left "Singleton set had no elements?!?"
            Just (v, _)
                | v `S.member` uniqs -> Left "Duplicate unique constraint!"
                | otherwise          -> Right $ S.insert v uniqs

    reduceConstraints :: Ord v => Set v -> Int -> Set v -> (Int, Set v)
    reduceConstraints solved n constraints
        | S.size constraints == 1 = (n, constraints)
        | S.size simplifiedConstraints > 1 = (n+1, simplifiedConstraints)
        | otherwise = (n, simplifiedConstraints)
      where
        simplifiedConstraints = S.difference constraints solved

    unsetify :: Set v -> Either String v
    unsetify s = case S.minView s of
        Nothing -> Left "Empty constraint solution!"
        Just (v, rest)
            | S.null rest -> Right v
            | otherwise -> Left "Unsolved constraint solution!"

digitsToConstraints :: [Digit] -> Either String (Map Segment (Set Segment))
digitsToConstraints ds = foldM updateConstraints unconstrained ds
  where
    original :: Map Int (Set Segment, Set Segment)
    original = digitSetToSegmentSets <$>
        digitsBySegmentCount (M.keys digitIntMap)

    scrambled :: Map Int (Set Segment, Set Segment)
    scrambled = digitSetToSegmentSets <$> digitsBySegmentCount ds

    allSegments :: Set Segment
    allSegments = S.fromList [A, B, C, D, E, F, G]

    unconstrained :: Map Segment (Set Segment)
    unconstrained = expandToMap allSegments allSegments

    updateConstraints
        :: Map Segment (Set Segment)
        -> Digit
        -> Either String (Map Segment (Set Segment))
    updateConstraints old (Digit s) = do
        (originalShared, originalUnique) <- case M.lookup idx original of
            Nothing -> Left $ "No digits with " ++ show idx ++ " segments!"
            Just v -> Right v

        (scrambledShared, scrambledUnique) <- case M.lookup idx scrambled of
            Nothing -> Left $ "No digits with " ++ show idx ++ " segments!"
            Just v -> Right v

        let constraints = mconcat
                [ expandToMap originalShared scrambledShared
                , expandToMap originalUnique scrambledUnique
                ]
        return $ M.unionWith S.intersection old constraints
      where
        idx :: Int
        idx = S.size s

translateDigits :: Map Segment Segment -> Digit -> Either String Digit
translateDigits translator (Digit s) =
    Digit . S.fromList <$> traverse translate (S.toList s)
  where
    translate :: Segment -> Either String Segment
    translate k = case M.lookup k translator of
        Nothing -> Left $ "No translation for: " ++ show k
        Just v -> Right v

reportDisplay :: ([Digit], [Digit]) -> Either String Int
reportDisplay (signals, display) = do
    solution <- digitsToConstraints signals >>= simplifyConstraints
    translation <- mapM (translateDigits solution >=> digitToInt) display
    return $ foldl' accumDigits 0 translation
  where
    accumDigits :: Int -> Int -> Int
    accumDigits acc n = 10 * acc + n

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile (many displayParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    let outputDigits = concatMap snd inputData
        isUniqueDigit (Digit s) = S.size s `elem` [2,3,4,7]

    print . length $ filter isUniqueDigit outputDigits

    case mapM reportDisplay inputData of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right l -> print $ sum l
