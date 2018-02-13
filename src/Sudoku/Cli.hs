module Sudoku.Cli where

import Sudoku.Validation
import Sudoku.BusinessLogic

app :: [String] -> String
app = inMemoryTransaction validate id solve show
