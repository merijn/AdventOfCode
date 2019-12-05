{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main(main) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector.Unboxed (Vector, (//))
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

newtype Interpreter a =
    Interpreter (ExceptT String (ReaderT (IOVector Int) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runInterpreter :: Interpreter a -> Vector Int -> IO a
runInterpreter (Interpreter int) vec = do
    intcodes <- V.thaw vec
    result <- runReaderT (runExceptT int) intcodes
    case result of
        Left err -> putStrLn err >> exitFailure
        Right r -> return r

readOffset :: Int -> Interpreter Int
readOffset n = Interpreter $ do
    vec <- ask
    V.read vec n

writeOffset :: Int -> Int -> Interpreter ()
writeOffset n val = Interpreter $ do
    vec <- ask
    V.write vec n val

errorMsg :: String -> Interpreter a
errorMsg = Interpreter . throwE

data Param = Indirect Int | Direct Int deriving (Eq, Show)

readParam :: Param -> Interpreter Int
readParam (Direct i) = return i
readParam (Indirect i) = readOffset i

data OpCode
    = Add Param Param Int
    | Mul Param Param Int
    | Halt
    deriving (Eq, Show)

decodeOpcode :: Int -> Interpreter OpCode
decodeOpcode addr = do
    (modes, opcode) <- (`divMod` 100) <$> readOffset addr
    case opcode of
        1 -> Add <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
        2 -> Mul <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
        99 -> return Halt
        i -> errorMsg $ "Unknown opcode: " ++ show i
  where
    posMode :: Int -> Int -> (Int -> Param)
    posMode modes n
        | 1 == (modes `div` 10^(n-1)) `mod` 10 = Direct
        | otherwise = Indirect

    param :: Int -> Int -> Interpreter Param
    param modes n = posMode modes n <$> readOffset (addr + n)

    outputParam :: Int -> Int -> Interpreter Int
    outputParam modes n = do
        p <- param modes n
        case p of
            Direct _ -> errorMsg "Output param can't be direct."
            Indirect i -> return i

interpreter :: Interpreter ()
interpreter = stepComputer 0
  where
    stepComputer :: Int -> Interpreter ()
    stepComputer n = do
        opcode <- decodeOpcode n
        case opcode of
            Add p1 p2 out -> do
                result <- (+) <$> readParam p1 <*> readParam p2
                writeOffset out result
                stepComputer (n + 4)

            Mul p1 p2 out -> do
                result <- (*) <$> readParam p1 <*> readParam p2
                writeOffset out result
                stepComputer (n + 4)

            Halt -> readOffset 0 >>= liftIO . print

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    case toOpcodes inputData of
        Left err -> putStrLn err >> exitFailure
        Right intcodes -> runInterpreter interpreter (intcodes // [(1,12),(2,2)])
