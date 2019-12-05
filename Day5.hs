{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
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

runIntcode :: Vector Int -> ExceptT String IO ()
runIntcode frozenIntcodes = do
    intcodes <- V.thaw frozenIntcodes
    V.write intcodes 1 12
    V.write intcodes 2 2
    stepComputer 0 intcodes
  where
    stepComputer
        :: Int -> IOVector Int -> ExceptT String IO ()
    stepComputer n vec = do
        opcode <- V.read vec n
        case opcode of
            1 -> performOp (+) >> stepComputer (n + 4) vec
            2 -> performOp (*) >> stepComputer (n + 4) vec
            99 -> V.read vec 0 >>= liftIO . print
            i -> throwE $ "Unknown opcode: " ++ show i
      where
        performOp :: (Int -> Int -> Int) -> ExceptT String IO ()
        performOp f = do
            input1 <- V.read vec (n + 1) >>= V.read vec
            input2 <- V.read vec (n + 2) >>= V.read vec
            outputIdx <- V.read vec (n + 3)
            V.write vec outputIdx (f input1 input2)

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    result <- runExceptT $ do
        intcodes <- except $ toOpcodes inputData
        runIntcode intcodes

    case result of
        Left err -> putStrLn err >> exitFailure
        Right r -> return r