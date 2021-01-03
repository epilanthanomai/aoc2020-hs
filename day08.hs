module Main where

import Control.Applicative ((<|>))
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , string
  )

import AocUtil
  ( testAndRun
  , linesOf
  , number
  )

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)

main :: IO ()
main = testAndRun instructions runProgram 5

instructions :: ReadP [Instruction]
instructions = linesOf instruction

instruction :: ReadP Instruction
instruction = do
  name <- string "acc" <|> string "jmp" <|> string "nop"
  char ' '
  arg <- argument
  return $ case name of
                 "acc" -> Acc arg
                 "jmp" -> Jmp arg
                 "nop" -> Nop arg

argument :: ReadP Int
argument = do
  sign <- char '+' <|> char '-'
  value <- number
  return $ case sign of
                 '+' -> value
                 '-' -> (- value)

runProgram :: [Instruction] -> Int
runProgram prog = go prog' (V.replicate (V.length prog') False) 0 0
  where
    prog' = V.fromList prog
    go prog visited pc acc =
      case visited !? pc of
        Just False ->
          let visited' = V.modify (\v -> MV.write v pc True) visited
              (pc', acc') = step prog pc acc
            in go prog visited' pc' acc'
        _ -> acc

step :: V.Vector Instruction -> Int -> Int -> (Int, Int)
step prog pc acc =
  case prog ! pc of
    Acc n -> (pc + 1, acc + n)
    Jmp n -> (pc + n, acc)
    Nop _ -> (pc + 1, acc)
