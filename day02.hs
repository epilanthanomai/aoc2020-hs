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
  ( testAndRun2
  , linesOf
  , number
  )

main :: IO ()
main = testAndRun2 inputData
                  (countValid validByCount) 2
                  (countValid validByPosition) 1
  where
    inputData = linesOf passwordRecord
    countValid strategy = length . filter strategy

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

validByCount :: PasswordRecord -> Bool
validByCount (PasswordRecord low high c password) =
  let count = length $ filter (== c) password
   in (low <= count) && (count <= high)

validByPosition :: PasswordRecord -> Bool
validByPosition (PasswordRecord low high c password) =
  let lowChar = password !! (low - 1)
      highChar = password !! (high - 1)
   in (lowChar == c || highChar == c) && not (lowChar == highChar)
