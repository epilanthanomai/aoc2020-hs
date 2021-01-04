module Main where

import Control.Applicative ((<|>))
import Control.Monad.Trans.State (State(..), execState, get, gets, put)
import Data.List ((\\), partition, union)
import Data.Tuple (swap)
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , string
  )

import AocUtil
  ( testAndRun2
  , linesOf
  , number
  , rfoldr
  )

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)

main :: IO ()
main = testAndRun2 instructions
                   runBasicProgram 5
                   runWithSwap 8

--
-- parsing
--

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

--
-- general execution
--

data ProgramState a = ProgramState
  { programInstructions :: V.Vector Instruction
  , programVisited :: V.Vector Bool
  , programCounter :: Int
  , programAccumulator :: Int
  , extraProgramState :: a
  }

runProgram :: State (ProgramState n) () -> n -> [Instruction] -> Int
runProgram step initialState prog =
  let finalState = execState go (ProgramState prog' visited 0 0 initialState)
      go = do
        state <- get
        let visited = programVisited state
            pc = programCounter state
        case visited !? pc of
          Just False -> do
            let visited' = V.modify (\v -> MV.write v pc True) visited
            put $ state { programVisited = visited' }
            step
            go
          _ -> return ()
      prog' = V.fromList prog
      visited = V.replicate (V.length prog') False
    in programAccumulator finalState

acc :: Int -> State (ProgramState n) ()
acc n = do
  state <- get
  let pc = programCounter state
      a = programAccumulator state
  put $ state { programCounter = pc + 1, programAccumulator = a + n }

jmp :: Int -> State (ProgramState n) ()
jmp n = do
  state <- get
  let pc = programCounter state
  put $ state { programCounter = pc + n }

nop :: State (ProgramState n) ()
nop = do
  state <- get
  let pc = programCounter state
  put $ state { programCounter = pc + 1 }

--
-- part 1
--

runBasicProgram :: [Instruction] -> Int
runBasicProgram = runProgram basicStep ()

basicStep :: State (ProgramState ()) ()
basicStep = do
  program <- gets programInstructions
  pc <- gets programCounter
  case program ! pc of
    Acc n -> acc n
    Jmp n -> jmp n
    Nop _ -> nop

--
-- part 2
--

type Index = Int
type Destination = Index
type JumpBlock = ([Index], Destination)
type JumpAnalysis = ([JumpBlock], [Index])

type SwapState = (Bool, [Index])

analyzeJumps :: [(Index, Instruction)] -> JumpAnalysis
analyzeJumps = foldr (uncurry collectInstruction) ([], [])
  where
    -- jmp creates a new block
    collectInstruction ix (Jmp n) (blocks, terminal) = (([ix], ix + n) : blocks, terminal)
    -- acc/nop without blocks are terminal
    collectInstruction ix _ ([], terminal) = ([], ix : terminal)
    -- acc/nop with block are added to the most recent block
    collectInstruction ix _ ((ixs, dest) : blocks, terminal) = ((ix : ixs, dest) : blocks, terminal)

recursiveTerminal :: Int -> JumpAnalysis -> [Index]
recursiveTerminal len (blocks, terminalByExec) =
  let (innerJumps, outerJumps) = partition (jumpIsInner . snd) blocks
      jumpIsInner dest = dest >= 0 && dest < len
      terminalByJump = mconcat $ map fst outerJumps
      directTerminal = union terminalByExec terminalByJump

      -- innerSources :: V.Vector [Int] -- all the instruction ixs that will
      --                                   reach this ix by one jump
      innerSources = V.accum (++) (V.replicate len []) $ map swap innerJumps
      ix +? acc = union acc [ix]
      newSources ix acc = innerSources ! ix \\ acc
   in rfoldr (+?) newSources [] directTerminal

runWithSwap :: [Instruction] -> Int
runWithSwap prog =
  let jumps = analyzeJumps $ zip [0..] prog
      terminals = recursiveTerminal (length prog) jumps
   in runProgram stepWithSwap (False, terminals) prog


useSwap :: State (ProgramState SwapState) ()
useSwap = do
  state <- get
  let (_, terminals) = extraProgramState state
  put $ state { extraProgramState = (True, terminals) }

stepWithSwap :: State (ProgramState SwapState) ()
stepWithSwap = do
  program <- gets programInstructions
  pc <- gets programCounter
  (swapUsed, terminals) <- gets extraProgramState
  case program ! pc of
    Acc n -> acc n
    Jmp n -> if not swapUsed && pc + 1 `elem` terminals
                then do { useSwap; nop }
                else jmp n
    Nop n -> if not swapUsed && pc + n `elem` terminals
                then do { useSwap; jmp n }
                else nop
