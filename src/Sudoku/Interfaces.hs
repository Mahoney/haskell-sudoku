module Sudoku.Interfaces where

import Data.List (intercalate)
import Data.List.Split (chunksOf)

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

data InitialValue = X | I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 deriving Enum

toCandidates:: InitialValue -> CandidateValues
toCandidates X = allCandidates
toCandidates v = [fromEnum v]

toSudoku :: [CandidateValues] -> Sudoku
toSudoku candidates =
  Sudoku (fmap (uncurry Cell) (zip allCoordinates candidates))

sudoku :: [InitialValue] -> Sudoku
sudoku vs =
  let asCandidates = fmap toCandidates vs
  in toSudoku asCandidates

cell :: Column -> Row -> CandidateValues -> Cell
cell c r = Cell (c, r)

allCandidates :: [Int]
allCandidates = [1..9]

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

rows :: Sudoku -> [[Cell]]
rows (Sudoku cells) = chunksOf 9 cells

instance Show Sudoku where
  show s =
    let rowStrings = fmap showRow (rows s)
        groupedRows = fmap (intercalate "") (chunksOf 3 rowStrings)
    in "\n+-------+-------+-------+" ++ intercalate "\n+-------+-------+-------+" groupedRows ++ "\n+-------+-------+-------+\n"

showRow :: [Cell] -> String
showRow cells =
  let cellsBySquare = fmap (intercalate "") (chunksOf 3 (fmap showCell cells))
  in "\n|" ++ intercalate " |" cellsBySquare ++ " |"

showCell :: Cell -> String
showCell (Cell _ [singleValue]) = " " ++ show singleValue
showCell _ = " ."
