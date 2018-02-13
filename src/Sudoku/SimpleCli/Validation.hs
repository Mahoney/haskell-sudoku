module Sudoku.SimpleCli.Validation (validate) where

import Sudoku.Interfaces
import Data.Char
import Data.List
import Ext.Data.Either

type ValidationError = String
type Validator = [String] -> Either ValidationError Sudoku

validate :: Validator
validate [arg] = validateSudoku arg
validate _ = Left "Takes one argument of type sudoku"

validateSudoku :: String -> Either ValidationError Sudoku
validateSudoku arg
  | length arg == 81 =
    let maybeValues =
          leftMap (intercalate "; ") (split (fmap toCellContents arg))
    in fmap toSudoku maybeValues
  | otherwise = Left (
      "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had "
      ++ show (length arg)
    )

toCellContents :: Char -> Either ValidationError CandidateValues
toCellContents char
  | char == '.'              = Right allCandidates
  | char `elem` ['1' .. '9'] = Right [digitToInt char]
  | otherwise                = Left (char : " is not . or 1-9")

toSudoku :: [CandidateValues] -> Sudoku
toSudoku candidates =
  fmap (uncurry Cell) (zip allCoordinates candidates)
