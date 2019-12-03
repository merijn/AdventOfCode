{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Either.Validation
    (Validation, eitherToValidation, validationToEither)
import Data.Foldable (asum)
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

runIntcode :: Vector Int -> Int -> Int -> Either String Int
runIntcode frozenIntcodes noun verb = runST $ do
    intcodes <- V.thaw frozenIntcodes
    V.write intcodes 1 12
    V.write intcodes 2 2
    stepComputer 0 intcodes
  where
    stepComputer
        :: forall s . Int -> STVector s Int -> ST s (Either String Int)
    stepComputer n vec = do
        opcode <- V.read vec n
        case opcode of
            1 -> performOp (+) >> stepComputer (n + 4) vec
            2 -> performOp (*) >> stepComputer (n + 4) vec
            99 -> Right <$> V.read vec 0
            i -> return . Left $ "Unknown opcode: " ++ show i
      where
        performOp :: (Int -> Int -> Int) -> ST s ()
        performOp f = do
            input1 <- V.read vec (n + 1) >>= V.read vec
            input2 <- V.read vec (n + 2) >>= V.read vec
            outputIdx <- V.read vec (n + 3)
            V.write vec outputIdx (f input1 input2)

reportError :: Either String a -> IO a
reportError result =  case result of
    Left err -> putStrLn err >> exitFailure
    Right r -> return r

mapFirstSuccess
    :: forall a b e . ([e] -> e) -> [a] -> (a -> Either e b) -> Either e b
mapFirstSuccess combineErrors l f = getResult . asum . map tryF $ l
  where
    tryF :: a -> Validation [e] b
    tryF = eitherToValidation . first pure . f

    getResult :: Validation [e] b -> Either e b
    getResult = first combineErrors . validationToEither

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    (puzzle1, puzzle2) <- reportError $ do
        intcodes <- toOpcodes inputData
        result1 <- runIntcode intcodes 12 2
        result2 <- mapFirstSuccess unlines nounVerbPairs $ \(noun, verb) -> do
            result <- runIntcode intcodes noun verb
            if result == 19690720
               then Right (100 * noun + verb)
               else Left $ mconcat
                        [ "Incorrect solution for noun "
                        , show noun, " and verb ", show verb
                        ]
        return (result1, result2)
    print puzzle1
    print puzzle2
  where
    nounVerbPairs = [(noun,verb) | noun <- [0..99], verb <- [0..99]]
