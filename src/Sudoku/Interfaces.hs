module Sudoku.Interfaces (
  Sudoku,
  CandidateValues,
  CellCoordinates,
  makeSudoku,
  makeSudokuFromSets,
  sudoku,
  getCells,
  Row(..),
  Column(..),
  Cell(..),
  InitialValue(..),
  rows,
  columns,
  squares,
  emptyGrid,
  empty,
  cell
  ) where

import Data.Strings (strPadLeft)
import Data.List (intercalate, groupBy, sortBy)
import Data.Set as Set (Set, fromList, toList, map, filter, findMax )
import Data.List.Split (chunksOf)
import Control.Applicative (liftA2)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Row = A | B | C | D | E | F | G | H | I
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type CandidateValues = Set Int

type CellCoordinates = (Row, Column)

data Cell = Cell {
              coordinates :: CellCoordinates,
              value :: CandidateValues
            }
            deriving (Eq, Ord)
instance Show Cell where
  show (Cell (row, col) candidates) = show row ++ show (fromEnum col + 1) ++ ":" ++ show (toList candidates)

newtype Sudoku = Sudoku (Set Cell) deriving Eq

data InitialValue = X | I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 deriving Enum

makeSudokuFromSets :: Set Cell -> Sudoku
makeSudokuFromSets = Sudoku

makeSudoku :: [Cell] -> Sudoku
makeSudoku cells = Sudoku (fromList cells)

getCells :: Sudoku -> Set Cell
getCells (Sudoku cells) = cells

toCandidates:: InitialValue -> CandidateValues
toCandidates X = allCandidates
toCandidates v = fromList [fromEnum v]

toSudoku :: [CandidateValues] -> Sudoku
toSudoku candidates =
  Sudoku (Set.map (uncurry Cell) (fromList (zip (toList allCoordinates) candidates)))

sudoku :: [InitialValue] -> Sudoku
sudoku vs =
  let asCandidates = fmap toCandidates vs
  in toSudoku asCandidates

cell :: Row -> Column -> [Int] -> Cell
cell r c ints = Cell (r, c) (fromList ints)

allCandidates :: Set Int
allCandidates = fromList [1..9]

empty :: Row -> Column -> Cell
empty r c = Cell (r, c) allCandidates

allCoordinates :: Set CellCoordinates
allCoordinates = fromList [ (row,col) | row<-[A ..],col<-[C1 ..] ]

emptyGrid :: Sudoku
emptyGrid = Sudoku (Set.map (`Cell` allCandidates) allCoordinates)

rows :: Sudoku -> Set (Set Cell)
rows (Sudoku cellSet) =
  let rs f (Cell (r1, _) _) (Cell (r2, _) _) = f r1 r2
      cells = toList cellSet
      rowsAsList = groupBy (rs (==)) . sortBy (rs compare) $ cells
  in fromList (fmap fromList rowsAsList)

columns :: Sudoku -> Set (Set Cell)
columns (Sudoku cellSet) =
  let cols f (Cell (_, col1) _) (Cell (_, col2) _) = f col1 col2
      cells = toList cellSet
      colsAsList = groupBy (cols (==)) . sortBy (cols compare) $ cells
  in fromList (fmap fromList colsAsList)

combinationsOf :: [a] -> [b] -> [(a,b)]
combinationsOf = liftA2 (,)

allSquareCoordinates :: Set (Set CellCoordinates)
allSquareCoordinates =
  let topLeftOfSquares = fromList (combinationsOf [A, D, G] [C1, C4, C7])
  in Set.map (\(col, row) -> fromList $ combinationsOf [col .. toEnum (fromEnum col + 2)] [row .. toEnum (fromEnum row + 2)]) topLeftOfSquares

squares :: Sudoku -> Set (Set Cell)
squares (Sudoku cells) =
  let f squareCoordinates =
        Set.filter (\c -> coordinates c `elem` squareCoordinates) cells
  in Set.map f allSquareCoordinates

maxCandidateValues :: Set Cell -> Int
maxCandidateValues cells = findMax (Set.map (length . value) cells)

instance Show Sudoku where
  show s =
    let padding = maxCandidateValues (getCells s) * 2
        rowStrings = fmap (showRow padding) (toList (rows s))
        groupedRows = chunksOf 3 rowStrings
        groupedRowStrings = fmap concat groupedRows
        squareTopOrBottom = replicate ((padding*3)+1) '-'
        horizontalSeparator = "\n+"++squareTopOrBottom++"+"++squareTopOrBottom++"+"++squareTopOrBottom++"+"
        sudokuBody = intercalate horizontalSeparator groupedRowStrings
    in horizontalSeparator ++
       sudokuBody ++
       horizontalSeparator++"\n"

showRow :: Int -> Set Cell -> String
showRow padding cellSet =
  let cells = toList cellSet
      cellStrings = fmap (showCell padding) cells
      cellsBySquare = fmap concat (chunksOf 3 cellStrings)
  in "\n|" ++ intercalate " |" cellsBySquare ++ " |"

showCell :: Int -> Cell -> String
showCell padding (Cell _ vals) = showVals padding (toList vals)

showVals :: Int -> [Int] -> String
showVals padding vals = strPadLeft ' ' padding (" " ++ (intercalate "," . fmap show) vals)
