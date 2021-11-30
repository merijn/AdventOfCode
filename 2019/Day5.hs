{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main(main) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
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
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

toOpcodes :: Text -> Either String (Vector Int)
toOpcodes = fmap V.fromList . mapM toDecimal . T.split (==',') . T.stripEnd
  where
    toDecimal :: Text -> Either String Int
    toDecimal txt = case T.signed T.decimal txt of
        Left t -> Left $ T.unpack txt ++ ": " ++ t
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
    | ReadIn Int
    | WriteOut Param
    | Jnz Param Param
    | Jez Param Param
    | Lt Param Param Int
    | Eq Param Param Int
    | Halt
    deriving (Eq, Show)

decodeOpcode :: Int -> Interpreter OpCode
decodeOpcode addr = do
    (modes, opcode) <- (`divMod` 100) <$> readOffset addr
    case opcode of
        1 -> Add <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
        2 -> Mul <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
        3 -> ReadIn <$> outputParam modes 1
        4 -> WriteOut <$> param modes 1
        5 -> Jnz <$> param modes 1 <*> param modes 2
        6 -> Jez <$> param modes 1 <*> param modes 2
        7 -> Lt <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
        8 -> Eq <$> param modes 1 <*> param modes 2 <*> outputParam modes 3
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

readInt :: IO Int
readInt = do
    putStr "Input integer: "
    hFlush stdout
    txt <- getLine
    case readMaybe txt of
        Just n -> return n
        Nothing -> do
            putStrLn "Not a valid integer!"
            readInt

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

            ReadIn out -> do
                liftIO readInt >>= writeOffset out
                stepComputer (n + 2)

            WriteOut param -> do
                readParam param >>= liftIO . print
                stepComputer (n + 2)

            Jnz cond target -> do
                val <- readParam cond
                offset <- readParam target
                if val /= 0
                   then stepComputer offset
                   else stepComputer (n + 3)

            Jez cond target -> do
                val <- readParam cond
                offset <- readParam target
                if val == 0
                   then stepComputer offset
                   else stepComputer (n + 3)

            Lt p1 p2 target -> do
                cond <- (<) <$> readParam p1 <*> readParam p2
                writeOffset target (if cond then 1 else 0)
                stepComputer (n + 4)

            Eq p1 p2 target -> do
                cond <- (==) <$> readParam p1 <*> readParam p2
                writeOffset target (if cond then 1 else 0)
                stepComputer (n + 4)

            Halt -> return ()

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> T.readFile inputFile
        _ -> putStrLn "No input file!" >> exitFailure

    case toOpcodes inputData of
        Left err -> putStrLn err >> exitFailure
        Right intcodes -> runInterpreter interpreter intcodes
