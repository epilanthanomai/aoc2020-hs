module Main where

import qualified Data.HashSet as HS
import Text.ParserCombinators.ReadP
  ( ReadP
  , many1
  , satisfy
  )

import AocUtil
  ( testAndRun2
  , blocksOf
  , linesOf
  )

type Question = Char
type Form = HS.HashSet Question
type Group = [Form]

main :: IO ()
main = testAndRun2 inputData
                   (groupQuestionCounts HS.union) 11
                   (groupQuestionCounts HS.intersection) 6
  where
    groupQuestionCounts combine = sum . map (questionsInGroup combine)
    questionsInGroup combine = length . foldr1 combine

inputData :: ReadP [Group]
inputData = blocksOf group

group :: ReadP Group
group = linesOf form

form :: ReadP Form
form = HS.fromList <$> many1 question

question :: ReadP Question
question = satisfy $ \c -> c >= 'a' && c <= 'z'
