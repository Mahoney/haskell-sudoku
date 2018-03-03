module Sudoku.BusinessLogic (
  inMemoryTransaction,
  solve
) where

import Sudoku.Interfaces
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map (fromList, unionWith, toList)
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
  let result = applyStrategyRecursively doWork sdku
  in if unsolvable result
       then []
       else [result]

doWork :: Sudoku -> Sudoku
doWork sdku = simplifyOverlapsBetweenSets (applyStrategyRecursively simplifyBySetInIsolation sdku)

applyStrategyRecursively :: (Sudoku -> Sudoku) -> Sudoku -> Sudoku
applyStrategyRecursively =
  untilPost (\old new -> ((new == old) || unsolvable new || solvedSudoku new))

untilPost :: (a -> a -> Bool) -> (a -> a) -> a -> a
untilPost p f old
  | p old new = new
  | otherwise = untilPost p f new
  where new = f old

simplifyBySetInIsolation :: Sudoku -> Sudoku
simplifyBySetInIsolation s = simplifyBySetInIsolation' rows (simplifyBySetInIsolation' columns (simplifyBySetInIsolation' squares s))

simplifyBySetInIsolation' :: (Sudoku -> Set (Set Cell)) -> Sudoku -> Sudoku
simplifyBySetInIsolation' setExtractor sdku =
  let sets = setExtractor sdku
      updatedSets = Set.map simplifySet sets
  in makeSudokuFromSets (unions (toList updatedSets))

simplifySet :: Set Cell -> Set Cell
simplifySet set =
  let sortedByNumberOfCandidates = sortBy (comparing (length . value)) (toList set)
  in foldl (\acc c -> removeImpossibleValuesFromCellsInSet (value c) acc) (fromList sortedByNumberOfCandidates) sortedByNumberOfCandidates

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

simplifyOverlapsBetweenSets :: Sudoku -> Sudoku
simplifyOverlapsBetweenSets sdku =
  let allSets = unions [squares sdku, rows sdku, columns sdku]
      changed = Set.map (simplifyOverlapsBetweenSets' allSets) allSets
  in merge sdku (unions $ toList changed)

simplifyOverlapsBetweenSets' :: Set (Set Cell) -> Set Cell -> Set Cell
simplifyOverlapsBetweenSets' allSets set =
  let unsolvedValues = unsolvedValueToCells set
      valuesToCellsThatShouldNotHaveIt' = valuesToCellsThatShouldNotHaveIt allSets unsolvedValues
      changed = Set.map (uncurry removeAndReturnChanged) valuesToCellsThatShouldNotHaveIt'
  in unions $ toList changed

unsolvedValueToCells :: Set Cell -> Set (Int, Set Cell)
unsolvedValueToCells set =
  let unsolvedCells = unsolved set
      unsolvedValues = unions (toList (Set.map value unsolvedCells))
  in Set.map (groupByCandidateValue unsolvedCells) unsolvedValues

groupByCandidateValue :: Set Cell -> Int -> (Int, Set Cell)
groupByCandidateValue cells val = (val, Set.filter (elem val . value) cells)

valuesToCellsThatShouldNotHaveIt :: Set (Set Cell) -> Set (Int, Set Cell) -> Set (Int, Set Cell)
valuesToCellsThatShouldNotHaveIt allSets = Set.map (`valueToCellsThatShouldNotHaveIt` allSets)

valueToCellsThatShouldNotHaveIt :: (Int, Set Cell) -> Set (Set Cell) -> (Int, Set Cell)
valueToCellsThatShouldNotHaveIt (val, cellsWithVal) allSets =
  let setsWithAllCells =
        Set.filter (\set -> cellsWithVal `isSubsetOf` set) allSets -- should find the set they came from originally and maybe one other
      cellsToChange = Set.map (\\ cellsWithVal) setsWithAllCells
  in (val, unions (toList cellsToChange))

removeAndReturnChanged :: Int -> Set Cell -> Set Cell
removeAndReturnChanged val cells =
  let cellsWithValue = Set.filter (elem val . value) cells
  in removeCandidates (singleton val) cellsWithValue
