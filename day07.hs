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
  ( assertEqual
  , parseFile
  , parseInputAndPrint
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
main = do
    parseFile inputData "samples/day07-a.txt" >>= assertEqual 4 . containerCount myBag
    parseFile inputData "samples/day07-b.txt" >>= assertEqual 126 . countBags myBag
    parseInputAndPrint inputData $ \contents -> (containerCount myBag contents, countBags myBag contents)
  where
    -- part 1
    containerCount color contents = HS.size $ recursiveLookup (containersMap contents) color
    containersMap = invert . stripCounts
    stripCounts = HM.map $ HS.fromList . map snd
    -- TODO: this seems like it should be a lot simpler...
    invert = fromPairs . fmap swap . toPairs
    toPairs = mconcat . map (traverse HS.toList) . HM.toList
    fromPairs = foldr (uncurry (HM.insertWith mappend)) HM.empty . (fmap . fmap) HS.singleton
    -- part 2
    countBags color contents = (rfoldr addCount (uncurry . nextBags $ contents) 0 [(1, color)]) - 1
    addCount = (+) . fst
    nextBags contents count color _ = map (mapFst (* count)) $ mlookup color contents

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
mlookup :: (Eq a, Hashable a, Monoid b) => a -> HM.HashMap a b -> b
mlookup k = maybe mempty id . HM.lookup k

-- Also not already a library function??
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f = swap . fmap f . swap

recursiveLookup :: SimpleContentsMap -> BagColor -> HS.HashSet BagColor
recursiveLookup contentsMap color = rfoldr HS.insert newParents firstParents firstParents
  -- NOTE: use firstParents as both the accumulator and the initial set of
  -- rfoldr. rfoldr folds its initial set into the accumulator, so this
  -- skips that.
  where firstParents = mlookup color contentsMap
        newParents = HS.difference . flip mlookup contentsMap
