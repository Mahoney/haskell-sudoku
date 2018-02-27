module Sudoku.BusinessLogic where

import Sudoku.Interfaces
import qualified Data.Set as Set
import Data.List
import Data.Ord
import Data.Maybe (fromJust, mapMaybe)
import Data.Map.Strict (fromList, unionWith, toList)
import Control.Arrow (second)

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

returnList :: (a -> b) -> (a -> [b])
returnList f a = [f a]

simplifyBySetInIsolation :: Sudoku -> Sudoku
simplifyBySetInIsolation s = simplifyBySetInIsolation' rows (simplifyBySetInIsolation' columns (simplifyBySetInIsolation' squares s))

simplifyBySetInIsolation' :: (Sudoku -> [[Cell]]) -> Sudoku -> Sudoku
simplifyBySetInIsolation' setExtractor sdku =
  let sets = setExtractor sdku
      updatedSets = fmap simplifySet sets
  in makeSudoku (concat updatedSets)

simplifySet :: [Cell] -> [Cell]
simplifySet cells =
  let sorted = sortBy (comparing (length . value)) cells
  in foldl (\acc c -> removeImpossibleValuesFromCellsInSet (value c) acc) sorted sorted

removeImpossibleValuesFromCellsInSet :: CandidateValues -> [Cell] -> [Cell]
removeImpossibleValuesFromCellsInSet interestingValues cells =
  let (onlyHasVals, others) =
        partition
          (\(Cell _ values) -> isSubsequenceOf values interestingValues)
          cells
  in if length onlyHasVals == length interestingValues
       then onlyHasVals ++ removeCandidates interestingValues others
       else cells

removeCandidates :: CandidateValues -> [Cell] -> [Cell]
removeCandidates cand = fmap (\(Cell coords values) -> Cell coords (values \\ cand))

solved :: Cell -> Bool
solved c = length (value c) == 1

solvedSudoku :: Sudoku -> Bool
solvedSudoku s = all solved (getCells s)

impossible :: Cell -> Bool
impossible c = null (value c)

unsolvable :: Sudoku -> Bool
unsolvable s = any unsolvableSet (rows s ++ columns s ++ squares s)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

unsolvableSet :: [Cell] -> Bool
unsolvableSet cells =
  let solvedValues = fmap (head . value) (filter solved cells)
  in any impossible cells || hasDuplicates solvedValues

unsolved :: [Cell] -> [Cell]
unsolved = filter (not . solved)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

overlapStrategy :: Sudoku -> Sudoku
overlapStrategy sdku = rowColumnOverlapStrategy rows (rowColumnOverlapStrategy columns sdku)

rowColumnOverlapStrategy :: (Sudoku -> [[Cell]]) -> Sudoku -> Sudoku
rowColumnOverlapStrategy setExtractor sdku =
                   let sets = setExtractor sdku
                   in foldl removeOverlapRowsColumns sdku sets

removeOverlapRowsColumns :: Sudoku -> [Cell] -> Sudoku
removeOverlapRowsColumns sdku set =
  let unsolvedCells = unsolved set
      unsolvedValues = removeDuplicates (concatMap value unsolvedCells)
      unsolvedValuesToCoordsOfSingleSquareInWichTheyOccur =
        removeEmpties $ fmap (valueToAffectedSquare unsolvedCells) unsolvedValues
      unsolvedValuesToActualSquareInWhichTheyOccur =
        mapValue (findSquare sdku) unsolvedValuesToCoordsOfSingleSquareInWichTheyOccur
      updatedCellSets = fmap (\(val, square) -> cleanseThoseNotIn set square val) unsolvedValuesToActualSquareInWhichTheyOccur
  in merge sdku $ concat updatedCellSets

cleanseThoseNotIn :: [Cell] -> [Cell] -> Int -> [Cell]
cleanseThoseNotIn setShouldHaveValues setShouldNotHaveValues val =
  let cellsToChange = setShouldNotHaveValues \\ setShouldHaveValues
  in removeCandidates [val] cellsToChange

valueToAffectedSquare :: [Cell] -> Int -> (Int, Maybe CellCoordinates)
valueToAffectedSquare unsolvedCells val =
  let cellsWithValue = filter (elem val . value) unsolvedCells
      coordsWithValue = fmap coordinates cellsWithValue
      square = coordsInSameSquare coordsWithValue
  in (val, square)

coordsInSameSquare :: [CellCoordinates] -> Maybe CellCoordinates
coordsInSameSquare coords =
  let bySquare = groupBy inSameSquare coords
      allInSameSquare = length bySquare == 1

  in if allInSameSquare then Just (head (head bySquare)) else Nothing

inSameSquare :: CellCoordinates -> CellCoordinates -> Bool
inSameSquare (col1, row1) (col2, row2) = inSameSquare' col1 col2 && inSameSquare' row1 row2

inSameSquare' :: (Enum a, Show a) => a -> a -> Bool
inSameSquare' c1 c2 = (fromEnum c1 `div` 3) == (fromEnum c2 `div` 3)

findSquare :: Sudoku -> CellCoordinates -> [Cell]
findSquare sdku cellCoords =
  fromJust $ find (elem cellCoords . fmap coordinates) (squares sdku)

merge :: Sudoku -> [Cell] -> Sudoku
merge sdku changedCells =
  let original = fromList (fmap cellToTuple (getCells sdku))
      changed = fromList (fmap cellToTuple changedCells)
      merged = unionWith (\_ cell2 -> cell2) original changed
  in makeSudoku $ fmap tupleToCell (toList merged)

cellToTuple :: Cell -> (CellCoordinates, CandidateValues)
cellToTuple (Cell coords values) = (coords, values)

tupleToCell :: (CellCoordinates, CandidateValues) -> Cell
tupleToCell (coords, values) = Cell coords values

toMaybe :: (a, Maybe b) -> Maybe (a, b)
toMaybe (key, Just val) = Just (key, val)
toMaybe (_, Nothing) = Nothing

removeEmpties :: [(a, Maybe b)] -> [(a, b)]
removeEmpties = mapMaybe toMaybe

mapValue :: (b -> c) -> [(a, b)] -> [(a, c)]
mapValue f = fmap (second f)
