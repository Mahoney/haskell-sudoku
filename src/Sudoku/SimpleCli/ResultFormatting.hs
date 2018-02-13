module Sudoku.SimpleCli.ResultFormatting where

import Sudoku.Interfaces

format :: [Sudoku] -> String
format [] = "Not a valid sudoku; no solutions possible"
format [valid] = show valid
format xs = let examples = take 5 xs
 in "Not a valid sudoku; at least " ++ show (length examples) ++ " solutions\n" ++ show examples
