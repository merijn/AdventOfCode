{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad (forM_, replicateM)
import Data.Bifunctor (bimap)
import Data.Bits (FiniteBits, finiteBitSize, setBit, testBit, zeroBits)
import Data.Char (digitToInt, isHexDigit)
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Megaparsec

type Parser = Parsec Void BitStream

toBitStream :: Text -> Maybe BitStream
toBitStream txt
    | T.all isHexDigit strippedText = Just (BitStream 0 strippedText)
    | otherwise = Nothing
  where
    strippedText = T.strip txt

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile

    inputStream <- case toBitStream inputTxt of
        Nothing -> hPutStrLn stderr "Invalid input stream!" >> exitFailure
        Just s -> return s

    case runParser parser inputFile inputStream of
        Right r -> return r
        Left e -> do
            forM_ (bundleErrors e) $ \err -> do
                putStrLn (parseErrorTextPretty err)
            exitFailure

parseCommandLine :: String -> Parser a -> IO a
parseCommandLine input parser = do
    inputStream <- case toBitStream (T.pack input) of
        Nothing -> hPutStrLn stderr "Invalid input stream!" >> exitFailure
        Just s -> return s

    case runParser parser "" inputStream of
        Right r -> return r
        Left e -> do
            forM_ (bundleErrors e) $ \err -> do
                putStrLn (parseErrorTextPretty err)
            exitFailure

newtype Bit = Bit { getBit :: Bool } deriving (Eq, Ord, Show)

{-# COMPLETE Zero, One #-}
pattern Zero :: Bit
pattern Zero = Bit False

pattern One :: Bit
pattern One = Bit True

bitToChar :: Bit -> Char
bitToChar Zero = '0'
bitToChar One = '1'

bitsToIntegral
    :: forall a m . (FiniteBits a, Integral a, MonadFail m) => [Bit] -> m a
bitsToIntegral bits
    | finiteBitSize result >= length bits = return result
    | otherwise = fail "Insufficient bits in result type!"
  where
    result :: a
    result = foldl' updateBit zeroBits $ zip [0..] (reverse bits)

    updateBit :: a -> (Int, Bit) -> a
    updateBit v tup = case tup of
        (_, Zero) -> v
        (n, One) -> setBit v n

data BitStream =
  BitStream { streamOffset :: {-# UNPACK #-} !Int, streamContent :: !Text }

hexCharToBits :: Char -> [Bit]
hexCharToBits c = foldMap (\n -> [hexCharToBit n c]) [0..3]

hexCharToBit :: Int -> Char -> Bit
hexCharToBit n c = Bit $ digitToInt c `testBit` (3 - n)

instance Stream BitStream where
    type Token BitStream = Bit
    type Tokens BitStream = [Bit]

    tokensToChunk :: Proxy BitStream -> [Token BitStream] -> Tokens BitStream
    tokensToChunk _ = id

    chunkToTokens :: Proxy BitStream -> Tokens BitStream -> [Token BitStream]
    chunkToTokens _ = id

    chunkLength :: Proxy BitStream -> Tokens BitStream -> Int
    chunkLength _ = length

    take1_ :: BitStream -> Maybe (Token BitStream, BitStream)
    take1_ BitStream{..} =
        bimap (hexCharToBit streamOffset) newStream <$> T.uncons streamContent
      where
        newStream :: Text -> BitStream
        newStream rest
            | streamOffset < 3 = BitStream (streamOffset + 1) streamContent
            | otherwise = BitStream 0 rest

    takeN_ :: Int -> BitStream -> Maybe (Tokens BitStream, BitStream)
    takeN_ n BitStream{..}
        | T.null streamContent = Nothing
        | otherwise = Just
            (prefixBits ++ remainderBits, BitStream newOffset remainder)
      where
        (fullChars, newOffset) = n `quotRem` 4
        (prefix, remainder) = T.splitAt fullChars streamContent

        prefixBits :: [Bit]
        prefixBits = concatMap hexCharToBits (T.unpack prefix)

        remainderBits :: [Bit]
        remainderBits = case T.uncons remainder of
            Nothing -> []
            Just (c, _) -> map (\b -> hexCharToBit b c) [0..newOffset-1]

    takeWhile_
        :: (Token BitStream -> Bool)
        -> BitStream
        -> (Tokens BitStream, BitStream)
    takeWhile_ p stream@BitStream{..} =
      case checkFirstChar streamOffset streamContent of
        Nothing -> ([], stream)
        Just ([], _) -> ([], stream)
        Just v -> checkRest v
      where
        checkFirstChar :: Int -> Text -> Maybe ([Bit], Text)
        checkFirstChar n txt = case T.uncons txt of
            Nothing -> Nothing
            Just (c, cs) -> Just (takeWhile p (drop n (hexCharToBits c)), cs)

        bitsAppend :: Char -> [Bit] -> [Bit]
        bitsAppend c t = hexCharToBits c ++ t

        findPrefix :: Text -> (Text, Text)
        findPrefix txt
            | p Zero && p One = (txt, "")
            | p Zero = T.span (=='0') txt
            | p One = T.span (\c -> c == 'f' || c == 'F') txt
            | otherwise = ("", txt)

        checkRest :: ([Bit], Text) -> ([Bit], BitStream)
        checkRest (prefixBits, streamTxt) = (fullBitSequence, streamRest)
          where
            (matching, rest) = findPrefix streamTxt
            (postfixBits, postfix) = case checkFirstChar 0 rest of
                Nothing -> ([], rest)
                Just (bits, txt) -> (bits, txt)

            streamRest :: BitStream
            streamRest = BitStream (length postfixBits) postfix

            fullBitSequence :: [Bit]
            fullBitSequence =
              prefixBits ++ T.foldr bitsAppend postfixBits matching

instance VisualStream BitStream where
    showTokens :: Proxy BitStream -> NonEmpty (Token BitStream) -> String
    showTokens _ = foldMap (pure . bitToChar)

data Header =
  Header
    { headerVersion :: {-# UNPACK #-} !Int
    , headerType :: {-# UNPACK #-} !Int
    } deriving (Show)

data Packet
    = LiteralPacket Header Int64
    | OperatorPacket Header [Packet]
    deriving (Show)

zeroP :: Parser Bit
zeroP = satisfy (== Zero)

oneP :: Parser Bit
oneP = satisfy (== One)

bitP :: Parser Bit
bitP = anySingle

sizedIntegralP :: forall a . (FiniteBits a, Integral a) => Int -> Parser a
sizedIntegralP n = count n bitP >>= bitsToIntegral

headerParser :: Parser Header
headerParser = do
    version <- count 3 bitP >>= bitsToIntegral
    ty <- count 3 bitP >>= bitsToIntegral
    return $ Header version ty

literalPayloadParser :: Parser Int64
literalPayloadParser = do
    bits <- chunks <> endParser
    bitsToIntegral bits
  where
    chunks :: Parser [Bit]
    chunks = mconcat <$> many chunkParser

    chunkParser :: Parser [Bit]
    chunkParser = oneP *> count 4 bitP

    endParser :: Parser [Bit]
    endParser = zeroP *> count 4 bitP

data OperatorLength = Bits Int | Packets Int deriving (Show)

operatorPayloadParser :: Parser [Packet]
operatorPayloadParser = do
    lenType <- bitP
    len <- case lenType of
        Zero -> Bits <$> sizedIntegralP 15
        One -> Packets <$> sizedIntegralP 11

    subPacketParser len

subPacketParser :: OperatorLength -> Parser [Packet]
subPacketParser (Packets n) = replicateM n packetParser
subPacketParser (Bits n) = go n
  where
    go :: Int -> Parser [Packet]
    go o | o > 0 = do
        start <- getOffset
        p <- packetParser
        end <- getOffset

        (:) p <$> go (o - (end - start))

    go 0 = return []
    go _ = fail "Consumed too much input!"

packetParser :: Parser Packet
packetParser = do
    hdr@Header{..} <- headerParser
    if headerType == 4
       then LiteralPacket hdr <$> literalPayloadParser
       else OperatorPacket hdr <$> operatorPayloadParser

sumVersions :: Packet -> Int
sumVersions (LiteralPacket Header{..} _) = headerVersion
sumVersions (OperatorPacket Header{..} ps) =
  headerVersion + sum (map sumVersions ps)

evalPacket :: Packet -> Int64
evalPacket (LiteralPacket _ v) = v
evalPacket (OperatorPacket Header{..} ps) = case headerType of
    0 -> sum subValues
    1 -> product subValues
    2 -> minimum subValues
    3 -> maximum subValues
  where
    subValues = map evalPacket ps

main :: IO ()
main = do
    args <- getArgs
    packet <- case args of
        [inputFile] -> parseCommandLine inputFile (packetParser <* skipMany zeroP <* eof)
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print packet
    print $ sumVersions packet
    print $ evalPacket packet
