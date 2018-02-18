module Sudoku.SimpleCli.Cli where

import Sudoku.SimpleCli.Validation (validate)
import Sudoku.SimpleCli.ResultFormatting (format)
import Sudoku.BusinessLogic (inMemoryTransaction, solve)

app :: [String] -> String
app = inMemoryTransaction validate id solve format
