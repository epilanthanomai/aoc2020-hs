module Main where

import Control.Applicative ((<|>))
import Data.List (union, delete)
import Data.Maybe (maybe)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , munch1
  , string
  )

import AocUtil
  ( assertEqual
  , parseFile
  , parseInputAndPrint
  , linesOf
  , number
  , separated
  , rfoldr
  )

type BagColor = String
type BagCount = (Int, BagColor)
type Contents = (BagColor, [BagCount])
type ContentsMap = [(BagColor, [BagCount])]

main :: IO ()
main = do
    parseFile inputData "samples/day07-a.txt" >>= assertEqual 4 . containerCount myBag
    parseFile inputData "samples/day07-b.txt" >>= assertEqual 126 . countBags myBag
    parseInputAndPrint inputData $ \contents -> (containerCount myBag contents, countBags myBag contents)

myBag :: BagColor
myBag = "shiny gold"

inputData :: ReadP ContentsMap
inputData = linesOf contents

contents :: ReadP Contents
contents = do
  container <- bagColor
  string " bags contain "
  contents <- bagContents
  char '.'
  return (container, contents)

bagColor :: ReadP String
bagColor = do
  modifier <- bagWord
  space <- string " "
  color <- bagWord
  return $ modifier ++ space ++ color

bagWord :: ReadP String
bagWord = munch1 $ \c -> c >= 'a' && c <= 'z'

bagContents :: ReadP [BagCount]
bagContents = separated (string ", ") singleContent <|> noContents

singleContent :: ReadP BagCount
singleContent = do
  count <- number
  char ' '
  color <- bagColor
  string " bag" <|> string " bags"
  return (count, color)

noContents :: ReadP [BagCount]
noContents = string "no other bags" *> return []

containerCount :: String -> ContentsMap -> Int
containerCount color = length . delete color . recursiveContainers color . collectAllContainers
  where
    recursiveContainers color containers = rfoldr (union . (:[])) (recursiveContainers' containers) [] [color]
    recursiveContainers' containers color _ = mlookup color containers
    collectAllContainers = invert . stripCounts
    invert = collectPairs . map swap . expandPairs
    expandPairs simpleContents = [ (a, b) | (a, bs) <- simpleContents, b <- bs ]
    collectPairs = foldr (uncurry collectPairs') []
    collectPairs' key value [] = [(key, [value])]
    collectPairs' key value (head@(accKey, accValue) : rest)
      | key == accKey = (accKey, accValue `union` [value]) : rest
      | otherwise = head : collectPairs' key value rest
    stripCounts = map (fmap $ map snd)

countBags :: String -> ContentsMap -> Int
countBags color contents = countBags' contents [(1, color)] - 1
  where
    countBags' contents = rfoldr addCount (uncurry . nextBags $ contents) 0
    addCount = (+) . fst
    nextBags contents count color _ = map (mapFst (* count)) $ mlookup color contents

-- TODO: this seems like it should be some common library function
mlookup :: (Eq a, Monoid b) => a -> [(a, b)] -> b
mlookup k = maybe mempty id . lookup k

-- Also not already a library function??
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f = swap . fmap f . swap
