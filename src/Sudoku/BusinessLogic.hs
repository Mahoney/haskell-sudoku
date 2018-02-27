module Sudoku.BusinessLogic (
  inMemoryTransaction,
  solve,
  merge,
  mapValue,
  coordsInSameSquare,
  inSameSquare,
  valueToAffectedSquare
) where

import Sudoku.Interfaces
import Data.List (sortBy, groupBy, find)
import Data.Ord (comparing)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map.Strict as Map (fromList, unionWith, toList)
import Control.Arrow (second)
import Data.Set as Set (filter, Set, fromList, union, unions, map, toList, partition, isSubsetOf, (\\), elemAt, singleton)

inMemoryTransaction ::
  (unvalidatedInput -> Either validationFailure input) ->
  (validationFailure -> output) ->
  (input -> result) ->
  (result -> output) ->
  unvalidatedInput -> output
inMemoryTransaction validator validationFailedFormatter businessLogic resultFormatter unvalidatedInput =
  let validationResult = validator unvalidatedInput
      result           = fmap businessLogic validationResult
  in either validationFailedFormatter resultFormatter result

solve :: Sudoku -> [Sudoku]
solve sdku =
  let result = applyStrategies [simplifyBySetInIsolation, overlapStrategy] sdku
  in if result == [sdku] || null result
       then result
       else solve $ head result

applyStrategies :: [Sudoku -> Sudoku] -> Sudoku -> [Sudoku]
applyStrategies strategies sdku
  | unsolvable sdku = []
  | solvedSudoku sdku = [sdku]
  | null strategies = [sdku]
  | otherwise = applyStrategies' strategies sdku

applyStrategies' :: [Sudoku -> Sudoku] -> Sudoku -> [Sudoku]
applyStrategies' [] sdku = [sdku]
applyStrategies' (strategy:strategies) sdku =
  let simplified = applyStrategy strategy sdku
  in applyStrategies strategies simplified

applyStrategy :: (Sudoku -> Sudoku) -> Sudoku -> Sudoku
applyStrategy strategy sdku
    | unsolvable sdku || solvedSudoku sdku   = sdku
    | otherwise                              = let simplified = strategy sdku
                                               in if simplified == sdku then sdku else applyStrategy strategy simplified

simplifyBySetInIsolation :: Sudoku -> Sudoku
simplifyBySetInIsolation s = simplifyBySetInIsolation' rows (simplifyBySetInIsolation' columns (simplifyBySetInIsolation' squares s))

simplifyBySetInIsolation' :: (Sudoku -> Set (Set Cell)) -> Sudoku -> Sudoku
simplifyBySetInIsolation' setExtractor sdku =
  let sets = setExtractor sdku
      updatedSets = Set.map simplifySet sets
  in makeSudokuFromSets (unions (toList updatedSets))

simplifySet :: Set Cell -> Set Cell
simplifySet cells =
  let sorted = sortBy (comparing (length . value)) (toList cells)
  in foldl (\acc c -> removeImpossibleValuesFromCellsInSet (value c) acc) (fromList sorted) sorted

removeImpossibleValuesFromCellsInSet :: CandidateValues -> Set Cell -> Set Cell
removeImpossibleValuesFromCellsInSet interestingValues cells =
  let (onlyHasVals, others) =
        partition (\(Cell _ values) -> isSubsetOf values interestingValues) cells
  in if length onlyHasVals == length interestingValues
       then onlyHasVals `union` removeCandidates interestingValues others
       else cells

removeCandidates :: CandidateValues -> Set Cell -> Set Cell
removeCandidates cand = Set.map (\(Cell coords values) -> Cell coords (values \\ cand))

solved :: Cell -> Bool
solved c = length (value c) == 1

solvedSudoku :: Sudoku -> Bool
solvedSudoku s = all solved (getCells s)

impossible :: Cell -> Bool
impossible c = null (value c)

unsolvable :: Sudoku -> Bool
unsolvable s = any unsolvableSet (unions [rows s, columns s, squares s])

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = fromList list

unsolvableSet :: Set Cell -> Bool
unsolvableSet cells =
  let solvedValues =
        fmap (elemAt 0 . value) (toList (Set.filter solved cells))
  in any impossible cells || hasDuplicates solvedValues

unsolved :: Set Cell -> Set Cell
unsolved = Set.filter (not . solved)

overlapStrategy :: Sudoku -> Sudoku
overlapStrategy sdku = rowColumnOverlapStrategy rows (rowColumnOverlapStrategy columns sdku)

rowColumnOverlapStrategy :: (Sudoku -> Set (Set Cell)) -> Sudoku -> Sudoku
rowColumnOverlapStrategy setExtractor sdku =
                   let sets = setExtractor sdku
                   in foldl removeOverlapRowsColumns sdku sets

removeOverlapRowsColumns :: Sudoku -> Set Cell -> Sudoku
removeOverlapRowsColumns sdku set =
  let unsolvedCells = unsolved set
      unsolvedValues = unions (toList (Set.map value unsolvedCells))
      unsolvedValuesToCoordsOfSingleSquareInWichTheyOccur =
        removeEmpties $ Set.map (valueToAffectedSquare unsolvedCells) unsolvedValues
      unsolvedValuesToActualSquareInWhichTheyOccur =
        mapValue (findSquare sdku) unsolvedValuesToCoordsOfSingleSquareInWichTheyOccur
      updatedCellSets = Set.map (\(val, square) -> cleanseThoseNotIn set square val) unsolvedValuesToActualSquareInWhichTheyOccur
  in merge sdku $ unions (toList updatedCellSets)


cleanseThoseNotIn :: Set Cell -> Set Cell -> Int -> Set Cell
cleanseThoseNotIn setShouldHaveValues setShouldNotHaveValues val =
  let cellsToChange = setShouldNotHaveValues \\ setShouldHaveValues
  in removeCandidates (singleton val) cellsToChange

valueToAffectedSquare :: Set Cell -> Int -> (Int, Maybe CellCoordinates)
valueToAffectedSquare unsolvedCells val =
  let cellsWithValue = Set.filter (elem val . value) unsolvedCells
      coordsWithValue = Set.map coordinates cellsWithValue
      square = coordsInSameSquare coordsWithValue
  in (val, square)

coordsInSameSquare :: Set CellCoordinates -> Maybe CellCoordinates
coordsInSameSquare coords =
  let bySquare = groupBy inSameSquare (toList coords)
      allInSameSquare = length bySquare == 1

  in if allInSameSquare then Just (head (head bySquare)) else Nothing

inSameSquare :: CellCoordinates -> CellCoordinates -> Bool
inSameSquare (col1, row1) (col2, row2) = inSameSquare' col1 col2 && inSameSquare' row1 row2

inSameSquare' :: (Enum a, Show a) => a -> a -> Bool
inSameSquare' c1 c2 = (fromEnum c1 `div` 3) == (fromEnum c2 `div` 3)

findSquare :: Sudoku -> CellCoordinates -> Set Cell
findSquare sdku cellCoords =
  fromJust $ find (elem cellCoords . Set.map coordinates) (squares sdku)

merge :: Sudoku -> Set Cell -> Sudoku
merge sdku changedCells =
  let original = Map.fromList (fmap cellToTuple (toList (getCells sdku)))
      changed = Map.fromList (fmap cellToTuple (toList changedCells))
      merged = Map.unionWith (\_ cell2 -> cell2) original changed
  in makeSudoku $ fmap tupleToCell (Map.toList merged)

cellToTuple :: Cell -> (CellCoordinates, CandidateValues)
cellToTuple (Cell coords values) = (coords, values)

tupleToCell :: (CellCoordinates, CandidateValues) -> Cell
tupleToCell (coords, values) = Cell coords values

toMaybe :: (a, Maybe b) -> Maybe (a, b)
toMaybe (key, Just val) = Just (key, val)
toMaybe (_, Nothing) = Nothing

removeEmpties :: (Ord a, Ord b) => Set (a, Maybe b) -> Set (a, b)
removeEmpties withEmpties = fromList (mapMaybe toMaybe (toList withEmpties))

mapValue :: (Ord a, Ord b, Ord c) => (b -> c) -> Set (a, b) -> Set (a, c)
mapValue f = Set.map (second f)
