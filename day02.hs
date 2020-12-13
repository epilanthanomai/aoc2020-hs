module Main where

import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , get
  , munch1
  , satisfy
  , string
  )
import AocUtil
  ( testAndRun
  , linesOf
  , number
  ) 

main :: IO ()
main = testAndRun inputData countValid 2
  where
    inputData = linesOf passwordRecord
    countValid = length . filter passwordValid

data PasswordRecord = PasswordRecord Int Int Char String

passwordRecord :: ReadP PasswordRecord
passwordRecord = do
  low <- number
  char '-'
  high <- number
  char ' '
  c <- get
  string ": "
  pw <- password
  return $ PasswordRecord low high c pw

password :: ReadP String
password = munch1 $ \c -> c /= '\n'

passwordValid :: PasswordRecord -> Bool
passwordValid (PasswordRecord low high c password) =
  let count = length $ filter (== c) password
   in (low <= count) && (count <= high)
