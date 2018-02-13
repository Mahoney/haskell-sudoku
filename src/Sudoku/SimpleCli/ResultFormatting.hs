module Sudoku.SimpleCli.ResultFormatting where

import Sudoku.Interfaces

format :: [Sudoku] -> String
format [] = "Not a valid sudoku; no solutions possible"
format [valid] = show valid
format xs =
  let examples = take 5 xs
      numberOfExamples = length examples
      text = if numberOfExamples < 5 then "" else "at least "
  in "Not a valid sudoku; " ++ text ++ show numberOfExamples ++ " solutions\n" ++ show examples
