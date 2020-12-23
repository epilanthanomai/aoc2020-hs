module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isDigit, isHexDigit)
import Data.Maybe (isJust, listToMaybe)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , count
  , many
  , many1
  , munch1
  , satisfy
  , string
  )
import AocUtil
  ( testAndRun2
  , blocksOf
  , digitsInRange
  , isNumberInRange
  , linesOf1
  , number
  , parse
  , separated
  )

type FieldName = String
type FieldData = String
type FieldPair = (FieldName, FieldData)
type FieldBag = [FieldPair]

main :: IO ()
main = testAndRun2 inputData
                   (countValidPassports hasRequiredFields) 2
                   (countValidPassports hasValidFieldData) 2
  where countValidPassports check = length . filter check

--
-- file parsing
--

inputData :: ReadP [FieldBag]
inputData = blocksOf fieldBag

fieldBag :: ReadP FieldBag
fieldBag = mconcat <$> linesOf1 fields

fields :: ReadP FieldBag
fields =  separated (char ' ') fieldPair

fieldPair :: ReadP FieldPair
fieldPair = do
  fName <- fieldName
  char ':'
  fData <- fieldData
  return (fName, fData)

separators :: [Char]
separators = " \n:"

isSeparator :: Char -> Bool
isSeparator = (`elem` separators)

fieldName :: ReadP FieldName
fieldName = munch1 $ not . isSeparator

fieldData :: ReadP FieldData
fieldData = munch1 $ not . isSeparator

--
-- part 1: fields present
--

requiredFields :: [FieldName]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredFields :: FieldBag -> Bool
hasRequiredFields bag = all (hasRequiredField bag) requiredFields

hasRequiredField :: FieldBag -> FieldName -> Bool
hasRequiredField bag fName = isJust $ lookup fName bag

--
-- part 2: field value parsing
--

parseByr :: ReadP Int
parseByr = digitsInRange 4 1920 2002

parseIyr :: ReadP Int
parseIyr = digitsInRange 4 2010 2020

parseEyr :: ReadP Int
parseEyr = digitsInRange 4 2020 2030

-- We could parse this into a more useful result, but we don't need to: For
-- this problem we just need to verify that a parse is possible, without
-- using that value.
parseHgt :: ReadP (Int, String)
parseHgt = do
  n <- number
  unit <- string "cm" <|> string "in"
  let (low, high) = case unit of
                      "cm" -> (150, 193)
                      "in" -> (59, 76)
  guard $ isNumberInRange low high n
  return (n, unit)

parseHcl :: ReadP String
parseHcl = char '#' *> count 6 (satisfy isHexDigit)

eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parseEcl :: ReadP String
parseEcl = foldr1 (<|>) $ map string eyeColors

parsePid :: ReadP String
parsePid = count 9 $ satisfy isDigit

-- I wanted to use the (-> r) monad here and just define these as:
--   hasValidFieldData = isJust <$> do
--     validateField "byr" parseByr
--     [etc]
-- Apparently though, that triggers the (-> r) monad's behavior of
-- passing the bag to each, but it *doesn't* trigger the Maybe monad
-- functionality of stopping when it reaches a Nothing. Which makes sense:
-- monad execution should probably only collect effects from a single layer
-- of monadic wrapping, not two.
-- TODO: Figure out how to use the Maybe and (> r) monads together here.
hasValidFieldData :: FieldBag -> Bool
hasValidFieldData b = isJust $ do
  validateField "byr" parseByr b
  validateField "iyr" parseIyr b
  validateField "eyr" parseEyr b
  validateField "hgt" parseHgt b
  validateField "hcl" parseHcl b
  validateField "ecl" parseEcl b
  validateField "pid" parsePid b

validateField :: String -> ReadP a -> FieldBag -> Maybe ()
validateField fieldName p b = lookup fieldName b >>= parse p >> return ()
