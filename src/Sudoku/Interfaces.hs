module Sudoku.Interfaces where

import Data.List
import Data.List.Split

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Column = A | B | C | D | E | F | G | H | I
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type CandidateValues = [Int]

type CellCoordinates = (Column, Row)

data Cell = Cell {
              coordinates :: CellCoordinates,
              value :: CandidateValues
            }
            deriving (Eq, Ord, Show)

newtype Sudoku = Sudoku [Cell] deriving Eq

cell :: Column -> Row -> CandidateValues -> Cell
cell c r = Cell (c, r)

inked :: Column -> Row -> Int -> Cell
inked c r i = cell c r [i]

allCandidates :: [Int]
allCandidates = [1..9]

empty :: Column -> Row -> Cell
empty c r = cell c r allCandidates

allCoordinates :: [CellCoordinates]
allCoordinates = [ (col,row) | row<-[R1 ..],col<-[A ..] ]

emptyGrid :: Sudoku
emptyGrid = Sudoku (fmap (`Cell` allCandidates) allCoordinates)

solved :: Cell -> Bool
solved c = length (value c) == 1

impossible :: Cell -> Bool
impossible c = null (value c)

unsolveable :: Sudoku -> Bool
unsolveable (Sudoku cells) = any impossible cells

instance Show Sudoku where
  show (Sudoku cells) =
    let rows = fmap showRow (chunksOf 9 cells)
        groupedRows = fmap (intercalate "") (chunksOf 3 rows)
    in "\n+-------+-------+-------+" ++ intercalate "\n+-------+-------+-------+" groupedRows ++ "\n+-------+-------+-------+\n"

showRow :: [Cell] -> String
showRow cells =
  let cellsBySquare = fmap (intercalate "") (chunksOf 3 (fmap showCell cells))
  in "\n|" ++ intercalate " |" cellsBySquare ++ " |"

showCell :: Cell -> String
showCell (Cell _ [singleValue]) = " " ++ show singleValue
showCell _ = " ."
