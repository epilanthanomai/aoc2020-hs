module Main where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , many
  )

import AocUtil
  ( testAndRun
  , linesOf
  )

data MapPoint = Clear | Tree deriving (Eq, Show)

main :: IO ()
main = testAndRun inputData (countTreesInPath 1 3) 7

inputData :: ReadP [[MapPoint]]
inputData = linesOf mapRow

mapRow :: ReadP [MapPoint]
mapRow = many mapPoint

mapPoint :: ReadP MapPoint
mapPoint = clear <|> tree

clear :: ReadP MapPoint
clear = char '.' >> return Clear

tree :: ReadP MapPoint
tree = char '#' >> return Tree

countTreesInPath :: Int -> Int -> [[MapPoint]] -> Int
countTreesInPath down right [] = 0
countTreesInPath down right terrain = countWhere (== Tree) $ getPath down 0 right 0 terrain

countWhere p = length . filter p

getPath :: Int -> Int -> Int -> Int -> [[MapPoint]] -> [MapPoint]
getPath _ _ _ _ [] = []
getPath down 0 right rightPos terrain@(row : _) =
  let nextRight = mod (rightPos + right) (length row)
      thisPoint = (head terrain) !! rightPos
   in thisPoint : getPath down down right nextRight terrain
getPath down downPos right rightPos (row : rest) =
  getPath down (downPos - 1) right rightPos rest
