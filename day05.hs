module Main where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.List (foldl', sort)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , many1
  )

import AocUtil
  ( assertEqual
  , parseInputAndPrint
  , parse
  , linesOf
  )

data SeatRowElement = FrontSeat | BackSeat deriving (Show, Eq, Ord)
data SeatColumnElement = LeftSeat | RightSeat deriving (Show, Eq, Ord)
type SeatRowSpecifier = [SeatRowElement]
type SeatColumnSpecifier = [SeatColumnElement]
data Seat = Seat SeatRowSpecifier SeatColumnSpecifier deriving (Show, Eq, Ord)

main :: IO ()
main = do
    mapM_ (uncurry runTest) testCases
    parseInputAndPrint inputData problemOutputs
  where
    problemOutputs seats = (maxSeatId seats, firstMissingSeatId seats)
    maxSeatId = foldr1 max . map seatId
    firstMissingSeatId = firstMissing . sort . map seatId

runTest :: String -> Int -> IO ()
runTest seatString expectSeatId =
  assertEqual (Just expectSeatId) $ parse (seatId <$> seat) seatString

testCases :: [(String, Int)]
testCases =
  [ ("BFFFBBFRRR", 567)
  , ("FFFBBBFRRR", 119)
  , ("BBFFBBFRLL", 820)
  ]

inputData :: ReadP [Seat]
inputData = linesOf seat

seat :: ReadP Seat
seat = Seat <$> many1 rowElement <*> many1 columnElement

seatElement :: Char -> a -> ReadP a
seatElement c result = char c *> return result

rowElement :: ReadP SeatRowElement
rowElement = seatElement 'F' FrontSeat <|> seatElement 'B' BackSeat

columnElement :: ReadP SeatColumnElement
columnElement = seatElement 'L' LeftSeat <|> seatElement 'R' RightSeat

collectBits :: Num n => (a -> n) -> [a] -> n
collectBits toBit = foldl' appendBit 0
  where appendBit n bitSpec = n * 2 + (toBit bitSpec)

bitPair :: (Eq a, Num n) => a -> a -> a -> n
bitPair a b v
  | v == a = 0
  | v == b = 1

rowId :: Num n => SeatRowSpecifier -> n
rowId = collectBits $ bitPair FrontSeat BackSeat

columnId :: Num n => SeatColumnSpecifier -> n
columnId = collectBits $ bitPair LeftSeat RightSeat

seatId :: Num n => Seat -> n
seatId (Seat rowSpec columnSpec) = (rowId rowSpec) * 8 + (columnId columnSpec)

firstMissing :: (Eq a, Enum a) => [a] -> a
firstMissing (n : rest) = either id succ $ foldM checkNext n rest
  where checkNext a b = let a' = succ a in (if a' == b then Right else Left) a'
