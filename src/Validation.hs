module Validation where

import Interfaces

validate :: Validator
validate [args] = Right (Sudoku args)
validate _ = Left "Takes one argument of type sudoku"
