module Sudoku.Interfaces where

import Data.List (intercalate, groupBy, sortBy)
import Data.List.Split (chunksOf)
import Control.Applicative (liftA2)

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

empty :: Column -> Row -> Cell
empty c r = cell c r allCandidates

allCoordinates :: [CellCoordinates]
allCoordinates = [ (col,row) | row<-[R1 ..],col<-[A ..] ]

emptyGrid :: Sudoku
emptyGrid = Sudoku (fmap (`Cell` allCandidates) allCoordinates)

rows :: Sudoku -> [[Cell]]
rows (Sudoku cells) = chunksOf 9 cells

columns :: Sudoku -> [[Cell]]
columns (Sudoku cells) =
  let cols f (Cell (col1, _) _) (Cell (col2, _) _) = f col1 col2
  in groupBy (cols (==)) . sortBy (cols compare) $ cells

topLeftOfSquares :: [CellCoordinates]
topLeftOfSquares =
  let combinationsOf = liftA2 (,)
  in combinationsOf [A, D, G] [R1, R4, R7]

allSquareCoordinates :: [[CellCoordinates]]
allSquareCoordinates =
  let combinationsOf = liftA2 (,)
  in fmap (\(col, row) -> combinationsOf [col .. toEnum (fromEnum col + 2)] [row .. toEnum (fromEnum row + 2)]) topLeftOfSquares

squares :: Sudoku -> [[Cell]]
squares (Sudoku cells) =
  let f squareCoordinates =
        filter (\c -> coordinates c `elem` squareCoordinates) cells
  in fmap f allSquareCoordinates

instance Show Sudoku where
  show s =
    let rowStrings = fmap showRow (rows s)
        groupedRows = chunksOf 3 rowStrings
        groupedRowStrings = fmap concat groupedRows
        sudokuBody = intercalate horizontalSeparator groupedRowStrings
    in horizontalSeparator ++
       sudokuBody ++
       horizontalSeparator++"\n"

horizontalSeparator :: String
horizontalSeparator = "\n+-------+-------+-------+"

showRow :: [Cell] -> String
showRow cells =
  let cellStrings = fmap showCell cells
      cellsBySquare = fmap concat (chunksOf 3 cellStrings)
  in "\n|" ++ intercalate " |" cellsBySquare ++ " |"

showCell :: Cell -> String
showCell (Cell _ [singleValue]) = " " ++ show singleValue
showCell _ = " ."
