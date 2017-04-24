module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ head args
  putStrLn (parseSolveAndShowSudoku (head args))

parseSolveAndShowSudoku :: String -> String
parseSolveAndShowSudoku = undefined

type Results = [Result]

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Column = A | B | C | D | E | F | G | H | I
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type CandidateValues = [Int]

data CellCoordinates = CellCoordinates {
                         column :: Column,
                         row :: Row
                       }
           deriving (Eq, Ord, Show, Read, Bounded)

data Cell = Cell {
              coordinates :: CellCoordinates,
              value :: CandidateValues
            }
           deriving (Eq, Ord, Show, Read)

type Grid = [Cell]
newtype Result = Result Grid deriving (Eq, Ord, Show, Read)
