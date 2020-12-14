module AocUtil
  ( testAndRun
  , testAndRun2
  , digits
  , digitsInRange
  , isNumberInRange
  , linesOf
  , number
  )
where

import Control.Monad (guard)
import Data.Char (isDigit)
import System.Environment (getProgName)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , count
  , eof
  , many
  , munch1
  , readP_to_S
  , satisfy
  )

import Paths_aoc2020 (getDataFileName)

--
-- problem main functionality
--

testAndRun :: (Eq b, Show b) => ReadP a -> (a -> b) -> b -> IO ()
testAndRun p f sampleResult = do
  progName <- getProgName
  parseAndVerify p f ("samples/" ++ progName ++ ".txt") sampleResult
  parseAndOutput p f ("input/" ++ progName ++ ".txt")

testAndRun2 :: (Eq b, Show b, Eq c, Show c) => ReadP a -> (a -> b) -> b -> (a -> c) -> c -> IO ()
testAndRun2 p f1 sampleResult1 f2 sampleResult2 = testAndRun p both (sampleResult1, sampleResult2)
  where both val = (f1 val, f2 val)

parseAndVerify :: (Eq b, Show b) => ReadP a -> (a -> b) -> FilePath -> b -> IO ()
parseAndVerify p f path val = do
  input <- parse p path
  let result = f input
  if result == val
     then return ()
     else error $ "Unexpected value from test input: " ++ (show result)

parseAndOutput :: Show b => ReadP a -> (a -> b) -> FilePath -> IO ()
parseAndOutput p f path = do
  input <- parse p path
  let result = f input
  putStrLn $ show result

--
-- parsing utilities
--

parse :: ReadP a -> FilePath -> IO a
parse p path = do
  inputContents <- getDataFileName path >>= readFile
  case readP_to_S p inputContents of
    [] -> error "Unable to parse data file"
    [(result, "")] -> return result
    _ -> error "Multiple parses of data file"

linesOf :: ReadP a -> ReadP [a]
linesOf p = many (p <* char '\n') <* eof

number :: (Integral n, Read n) => ReadP n
number = read <$> munch1 isDigit

digits :: (Integral n, Read n) => Int -> ReadP n
digits c = read <$> count 4 (satisfy isDigit)

isNumberInRange :: Integral n => n -> n -> n -> Bool
isNumberInRange low high year = low <= year && year <= high

digitsInRange :: Int -> Int -> Int -> ReadP Int
digitsInRange chars low high = do
  n <- digits chars
  guard $ isNumberInRange low high n
  return n

