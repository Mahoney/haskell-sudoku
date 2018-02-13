module Sudoku.SimpleCli.Cli where

import Sudoku.SimpleCli.Validation
import Sudoku.BusinessLogic

app :: [String] -> String
app = inMemoryTransaction validate id solve show
