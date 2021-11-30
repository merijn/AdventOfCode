{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Control.Monad (void)
import Data.Bifunctor (second)
import Data.Char (isSpace, isDigit, isHexDigit)
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (All(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Text.Megaparsec (Parsec, runParser)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

parseFile :: FilePath -> Parser a -> IO a
parseFile inputFile parser = do
    inputTxt <- T.readFile inputFile
    case runParser parser inputFile inputTxt of
        Left e -> putStrLn (errorBundlePretty e) >> exitFailure
        Right r -> return r

data DocumentField
    = BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PassportID
    | CountryID
    deriving (Show, Eq, Ord)

docField :: Parser DocumentField
docField = Mega.label "field type" $ asum
    [ BirthYear <$ Mega.string "byr"
    , IssueYear <$ Mega.string "iyr"
    , ExpirationYear <$ Mega.string "eyr"
    , Height <$ Mega.string "hgt"
    , HairColor <$ Mega.string "hcl"
    , EyeColor <$ Mega.string "ecl"
    , PassportID <$ Mega.string "pid"
    , CountryID <$ Mega.string "cid"
    ]

fieldParser :: Parser (DocumentField, Text)
fieldParser = Mega.label "document field" $ do
    field <- docField <* Mega.char ':'
    content <- Mega.takeWhile1P Nothing (not . isSpace)
    return (field, content)

docParser :: Parser (Map DocumentField Text)
docParser = Mega.label "document" $ do
    fields <- Mega.sepEndBy1 fieldParser docTerminator
    case sequence (M.fromListWithKey checkField (map (second Right) fields)) of
        Left err -> fail err
        Right v -> return v
  where
    docTerminator = asum [Mega.hspace1, void Mega.eol]

    checkField
        :: DocumentField
        -> Either String Text
        -> Either String Text
        -> Either String Text
    checkField field _ _ = Left $ "Duplicate field: " <> show field

blankLines :: Parser ()
blankLines = Mega.skipSome Mega.eol

multiDocParser :: Parser [Map DocumentField Text]
multiDocParser = Mega.sepEndBy1 docParser blankLines <* Mega.eof

isValidPassport1 :: Map DocumentField v -> Bool
isValidPassport1 = getAll . mconcat
    [ All . M.member BirthYear
    , All . M.member IssueYear
    , All . M.member ExpirationYear
    , All . M.member Height
    , All . M.member HairColor
    , All . M.member EyeColor
    , All . M.member PassportID
    ]

isValidPassport2 :: Map DocumentField Text -> Bool
isValidPassport2 doc = getAll $ foldMap checkField
    [ BirthYear
    , IssueYear
    , ExpirationYear
    , Height
    , HairColor
    , EyeColor
    , PassportID
    ]
  where
    checkField :: DocumentField -> All
    checkField f = case M.lookup f doc of
        Nothing -> All False
        Just txt -> validateField f $ T.unpack txt

validateField :: DocumentField -> String -> All
validateField field txt = All $ case field of
    BirthYear -> checkRange 1920 2020 txt
    IssueYear -> checkRange 2010 2020 txt
    ExpirationYear -> checkRange 2020 2030 txt
    EyeColor ->  S.member txt eyeColours
    PassportID -> length txt == 9 && all isDigit txt
    CountryID -> True
    HairColor -> case txt of
        '#':cs -> all isHexDigit cs
        _ -> False

    Height -> case span isDigit txt of
        (n, "cm") -> checkRange 150 193 n
        (n, "in") -> checkRange 59 76 n
        _ -> False
  where
    checkRange :: Int -> Int -> String -> Bool
    checkRange lo hi s = maybe False (\i -> i >= lo && i <= hi) $ readMaybe s

    eyeColours :: Set String
    eyeColours = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

main :: IO ()
main = do
    args <- getArgs
    inputData <- case args of
        [inputFile] -> parseFile inputFile multiDocParser
        _ -> hPutStrLn stderr "No input file!" >> exitFailure

    print . length $ filter isValidPassport1 inputData
    print . length $ filter isValidPassport2 inputData
