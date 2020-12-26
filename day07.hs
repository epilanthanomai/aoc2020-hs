module Main where

import Control.Applicative ((<|>))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (maybe)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , munch1
  , string
  )

import AocUtil
  ( testAndRun
  , linesOf
  , number
  , separated
  )

type BagColor = String
type BagCount = (Int, BagColor)
type Contents = (BagColor, [BagCount])
type ContentsMap = HM.HashMap BagColor [BagCount]
type SimpleContentsMap = HM.HashMap BagColor (HS.HashSet BagColor)

main :: IO ()
-- main = undefined
main = testAndRun inputData (containerCount myBag) 4
  where containerCount color contents = HS.size $ recursiveLookup (containersMap contents) color
        containersMap = invert . stripCounts
        stripCounts = HM.map $ HS.fromList . map snd
        -- TODO: this seems like it should be a lot simpler...
        invert = fromPairs . fmap swap . toPairs
        toPairs = mconcat . map (traverse HS.toList) . HM.toList
        fromPairs = foldr (uncurry (HM.insertWith mappend)) HM.empty . (fmap . fmap) HS.singleton

myBag :: BagColor
myBag = "shiny gold"

inputData :: ReadP ContentsMap
inputData = HM.fromList <$> linesOf contents

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

-- recursive foldr. for each value in ls, don't just fold it into acc: do
-- that and then call accumulated g on it and recursively fold those into
-- acc as well.
-- TODO: this seems like it's probably A Thing. is it in a library somwhere?
rfoldr :: (Foldable t1, Foldable t2) => (a -> b -> b) -> (a -> b -> t1 a) -> b -> t2 a -> b
rfoldr f g = foldr go
  where go v acc = let acc' = f v acc
                       ls' = g v acc'
                    in rfoldr f g acc' ls'

-- TODO: this seems like it should be some common library function over
-- HM.lookup...
setLookup :: (Eq a, Hashable a) => HM.HashMap a (HS.HashSet b) -> a -> HS.HashSet b
setLookup m k = maybe HS.empty id $ HM.lookup k m

recursiveLookup :: SimpleContentsMap -> BagColor -> HS.HashSet BagColor
recursiveLookup contentsMap color = rfoldr HS.insert newParents firstParents firstParents
  -- NOTE: use firstParents as both the accumulator and the initial set of
  -- rfoldr. rfoldr folds its initial set into the accumulator, so this
  -- skips that.
  where firstParents = setLookup contentsMap color
        newParents = HS.difference . setLookup contentsMap
