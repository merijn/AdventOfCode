{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (asum)
import Control.Monad (forM_, void)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor (first)
import Data.Char (isLetter)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup (stimes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, eol, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = ParsecT Void Text (State ProblemState)

parseFile :: FilePath -> Parser () -> IO Directory
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runState (runParserT parser inputFile inputTxt) initState of
        (Left e, _) -> putStrLn (errorBundlePretty e) >> exitFailure
        (Right (), PState{..}) -> return filesystem
  where
    initState :: ProblemState
    initState = PState [] (Dir M.empty)

data File = File { fileName :: Text, fileSize :: Int } deriving (Eq, Show)

data Directory = Dir (Map Text (Either Directory File))

data ProblemState = PState
    { cwd :: [Text]
    , filesystem :: Directory
    }

cd :: MonadState ProblemState m => ([Text] -> [Text]) -> m ()
cd f = State.modify' $ \pstate -> pstate { cwd = f (cwd pstate) }

createFile :: MonadState ProblemState m => File -> m ()
createFile file = do
    PState{..} <- State.get
    let path = reverse (fileName file : cwd)
    case insertFile file path filesystem of
        Left e -> error e
        Right v -> State.put $ PState cwd v

insertFile :: File -> [Text] -> Directory -> Either String Directory
insertFile file = go
  where
    insert
        :: Text
        -> [Text]
        -> Maybe (Either Directory File)
        -> Either String (Maybe (Either Directory File))
    insert name [] val = case val of
        Nothing -> Right (Just (Right file))
        Just _ -> Left $ T.unpack name ++ " already exists"

    insert name path val = case val of
        Nothing -> Just . Left <$> go path (Dir M.empty)
        Just (Left d) -> Just . Left <$> go path d
        Just (Right _) -> Left $ T.unpack name ++ " is a file"

    go :: [Text] -> Directory -> Either String Directory
    go [] _ = Left "can't happen"
    go (name:path) (Dir m) = Dir <$> M.alterF (insert name path) name m

isFileChar :: Char -> Bool
isFileChar c = isLetter c || c == '.'

cmdParser :: Parser ()
cmdParser = string "$ " >> asum
    [ string "cd /" *> eol *> cd (const [])
    , string "cd .." *> eol *> cd (drop 1)
    , do
        string "cd "
        takeWhileP Nothing isFileChar >>= cd . (:)
        void eol
    , listParser
    ]

listParser :: Parser ()
listParser = string "ls" >> eol >> skipSome (entryParser <* eol)
  where

    entryParser = dir <|> file
    dir = string "dir " >> skipSome letterChar
    file = do
        size <- decimal <* char ' '
        name <- takeWhileP Nothing isFileChar
        createFile $ File name size

outputParser :: Parser ()
outputParser = void $ skipSomeTill cmdParser eof

printDirTree :: Directory -> IO ()
printDirTree dirRoot = putStrLn "- / (dir)" >> go 1 dirRoot
  where
    go :: Int -> Directory -> IO ()
    go n (Dir dirMap) = forM_ (M.toList dirMap) $ \(name, val) -> do
        putStr $ stimes n "  " ++ "- " ++ T.unpack name
        case val of
            Left d -> putStrLn " (dir)" >> go (n+1) d
            Right File{..} -> putStrLn $ " (file, size=" ++ show fileSize ++ ")"

calculateSize :: Directory -> [(Text, Int)]
calculateSize = map (first concatPaths) . fst . dirSize
  where
    concatPaths :: [Text] -> Text
    concatPaths p = "/" <> T.intercalate "/" p

    dirSize :: Directory -> ([([Text], Int)], Int)
    dirSize (Dir dir) = (([], mySize) : dirSizes, mySize)
      where
        (dirSizes, sizeMap) = M.mapAccumWithKey helper [] dir
        mySize = sum sizeMap

    helper
        :: [([Text], Int)]
        -> Text
        -> Either Directory File
        -> ([([Text], Int)], Int)
    helper acc name val = case val of
        Left dir -> first accumulateDirs (dirSize dir)
        Right File{..} -> (acc, fileSize)
      where
        accumulateDirs :: [([Text], Int)] -> [([Text], Int)]
        accumulateDirs subdirs = map (first (name:)) subdirs ++ acc

main :: IO ()
main = do
    args <- getArgs
    return ()
    dirRoot <- case args of
        [inputFile] -> parseFile inputFile (outputParser <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    printDirTree dirRoot

    (totalSize, sizedDirs) <- case calculateSize dirRoot of
        (("/", totalSize) : sizedDirs) -> return (totalSize, sizedDirs)
        _ -> hPutStrLn stderr "No directory structure in input!" >> exitFailure


    let puzzle1Result = filter ((<=100000) . snd) sizedDirs

    putStrLn "\nPuzzle #1:"
    print $ sum (map snd puzzle1Result)

    let freeSpace = 70000000 - totalSize
        missingSpace = 30000000 - freeSpace
        largeDirs = dropWhile ((<missingSpace) . snd) $ sortOn snd sizedDirs

    putStrLn "\nPuzzle #2:"
    case largeDirs of
        [] -> putStrLn "No directory large enough to delete!"
        ((path, size):_) -> putStrLn $ T.unpack path ++ ": " ++ show size
