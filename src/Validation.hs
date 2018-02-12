module Validation where

import Interfaces

type ValidationError = String
type Validator = [String] -> Either ValidationError Sudoku

validate :: Validator
validate [args] = Right (Sudoku args)
validate _ = Left "Takes one argument of type sudoku"
