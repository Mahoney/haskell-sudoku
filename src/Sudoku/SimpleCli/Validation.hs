module Sudoku.SimpleCli.Validation (validate) where

import Sudoku.Interfaces (Sudoku, InitialValue(..), sudoku)
import Data.Char (digitToInt)
import Data.List (intercalate)
import Ext.Data.Either (leftMap, leftPartition)

type ValidationError = String
type Validator = [String] -> Either ValidationError Sudoku

validate :: Validator
validate [arg] = validateSudoku arg
validate _ = Left "Takes one argument of type sudoku"

validateSudoku :: String -> Either ValidationError Sudoku
validateSudoku arg
  | length arg == 81 =
    let eitherErrorsOrValues = leftPartition (fmap toCellContents arg)
        eitherSingleErrorOrValues = leftMap (intercalate "; ") eitherErrorsOrValues
    in fmap sudoku eitherSingleErrorOrValues
  | otherwise = Left (
      "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had "
      ++ show (length arg)
    )

toCellContents :: Char -> Either ValidationError InitialValue
toCellContents char
  | char == '.'              = Right X
  | char `elem` ['1' .. '9'] = Right (toEnum $ digitToInt char)
  | otherwise                = Left (char : " is not . or 1-9")
