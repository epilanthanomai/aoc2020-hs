module Main where

import qualified Data.HashSet as HS
import Text.ParserCombinators.ReadP
  ( ReadP
  , many1
  , satisfy
  )

import AocUtil
  ( testAndRun
  , blocksOf
  , linesOf
  )

type Question = Char
type Form = HS.HashSet Question
type Group = [Form]

main :: IO ()
main = testAndRun inputData groupQuestionCounts 11
  where
    groupQuestionCounts = sum . map questionsInGroup
    questionsInGroup = length . foldr1 HS.union

inputData :: ReadP [Group]
inputData = blocksOf group

group :: ReadP Group
group = linesOf form

form :: ReadP Form
form = HS.fromList <$> many1 question

question :: ReadP Question
question = satisfy $ \c -> c >= 'a' && c <= 'z'
