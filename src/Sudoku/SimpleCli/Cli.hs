module Sudoku.SimpleCli.Cli where

import Sudoku.SimpleCli.Validation
import Sudoku.SimpleCli.ResultFormatting
import Sudoku.BusinessLogic

app :: [String] -> String
app = inMemoryTransaction validate id solve format
