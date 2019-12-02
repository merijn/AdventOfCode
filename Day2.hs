{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Monad.ST (ST, runST)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as V
import System.Environment (getArgs)
import System.Exit (exitFailure)

toOpcodes :: Text -> Either String (Vector Int)
toOpcodes = fmap V.fromList . mapM toDecimal . T.split (==',') . T.stripEnd
  where
    toDecimal :: Text -> Either String Int
    toDecimal txt = case T.decimal txt of
        Left t -> Left t
        Right (t, remainder)
            | T.null remainder -> Right t
            | otherwise -> Left "Parse error!"

patchComputer :: STVector s Int -> ST s ()
patchComputer vec = do
    V.unsafeWrite vec 1 12
    V.unsafeWrite vec 2 2

runIntcode :: forall s . Vector Int -> ST s (Either String Int)
runIntcode frozenIntcodes = do
    intcodes <- V.unsafeThaw frozenIntcodes
    patchComputer intcodes
    stepComputer 0 intcodes
  where
    stepComputer :: Int -> STVector s Int -> ST s (Either String Int)
    stepComputer n vec = do
        opcode <- V.unsafeRead vec n
        case opcode of
            1 -> performOp (+) >> stepComputer (n + 4) vec
            2 -> performOp (*) >> stepComputer (n + 4) vec
            99 -> Right <$> V.unsafeRead vec 0
            i -> return . Left $ "Unknown opcode: " ++ show i
      where
        performOp :: (Int -> Int -> Int) -> ST s ()
        performOp f = do
            input1 <- V.unsafeRead vec (n + 1) >>= V.unsafeRead vec
            input2 <- V.unsafeRead vec (n + 2) >>= V.unsafeRead vec
            outputIdx <- V.unsafeRead vec (n + 3)
            V.unsafeWrite vec outputIdx (f input1 input2)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    case toOpcodes inputData of
        Left err -> putStrLn err >> exitFailure
        Right intcodes -> case runST (runIntcode intcodes) of
            Left err -> putStrLn err >> exitFailure
            Right result -> print result
