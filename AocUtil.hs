module AocUtil
  ( testAndRun
  , testAndRun2
  , linesOf
  , number
  )
where

import Data.Char (isDigit)
import System.Environment (getProgName)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , eof
  , many
  , munch1
  , readP_to_S
  )

import Paths_aoc2020 (getDataFileName)

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
