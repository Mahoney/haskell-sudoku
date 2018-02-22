module Sudoku.BusinessLogic where

import Sudoku.Interfaces (Sudoku(..), Cell(..), rows, columns, squares)
import qualified Data.Set as Set

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
solve sudoku = [sudoku]

solved :: Cell -> Bool
solved c = length (value c) == 1

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
