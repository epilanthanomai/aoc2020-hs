module AocUtil
  ( -- main utilities
    testAndRun
  , testAndRun2
  , parseSampleAndVerify
  , parseInputAndPrint
  , assertEqual
    -- parsing
  , blocksOf
  , digits
  , digitsInRange
  , isNumberInRange
  , linesOf
  , linesOf1
  , number
  , parse
  , separated
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
  , many1
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
  parseSampleAndVerify p f sampleResult
  parseInputAndPrint p f

testAndRun2 :: (Eq b, Show b, Eq c, Show c) => ReadP a -> (a -> b) -> b -> (a -> c) -> c -> IO ()
testAndRun2 p f1 sampleResult1 f2 sampleResult2 = testAndRun p both (sampleResult1, sampleResult2)
  where both val = (f1 val, f2 val)

parseSampleAndVerify :: (Eq b, Show b) => ReadP a -> (a -> b) -> b -> IO ()
parseSampleAndVerify p f val = do
  path <- defaultSampleFile
  input <- parseFile p path
  let result = f input
  assertEqual val result

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expect actual =
  if expect == actual
     then return ()
     else error $ "Unexpected value from test input: " ++ (show actual)


parseInputAndPrint :: Show b => ReadP a -> (a -> b) -> IO ()
parseInputAndPrint p f = do
  path <- defaultInputFile
  input <- parseFile p path
  let result = f input
  putStrLn $ show result

defaultSampleFile :: IO String
defaultSampleFile = do
  progName <- getProgName
  return $ "samples/" ++ progName ++ ".txt"

defaultInputFile :: IO String
defaultInputFile = do
  progName <- getProgName
  return $ "input/" ++ progName ++ ".txt"

--
-- parsing utilities
--

parseFile :: ReadP a -> FilePath -> IO a
parseFile p path = getDataFileName path >>= readFile >>= parse p

parse :: MonadFail m => ReadP a -> String -> m a
parse p s =
  case readP_to_S (p <* eof) s of
    [] -> fail "Unable to parse data file"
    [(result, "")] -> return result
    _ -> fail "Multiple parses of data file"

separated :: ReadP s -> ReadP a -> ReadP [a]
separated s p = (:) <$> p <*> many (s *> p)

linesOf :: ReadP a -> ReadP [a]
linesOf p = many $ p <* char '\n'

linesOf1 :: ReadP a -> ReadP [a]
linesOf1 p = many1 $ p <* char '\n'

blocksOf :: ReadP a -> ReadP [a]
blocksOf = separated $ char '\n'

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
