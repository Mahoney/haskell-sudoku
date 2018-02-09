module Main where

import System.Environment

type ValidationError = String
newtype Sudoku = Sudoku String deriving Show
type Validator = [String] -> Either ValidationError Sudoku
type Solver = Sudoku -> [Sudoku]

validate :: Validator
validate [args] = Right (Sudoku args)
validate _ = Left "Takes one argument of type sudoku"

solve :: Solver
solve sudoku = [sudoku]

buildApp :: Validator -> Solver -> [String] -> Either ValidationError [Sudoku]
buildApp validator solver args = fmap solver (validator args)

businessLogic :: [String] -> Either ValidationError [Sudoku]
businessLogic = buildApp validate solve

main :: IO ()
main = do
  args <- getArgs
  let result = businessLogic args
  let output =
        case result of
          Left validationError -> validationError
          Right sudoku -> "Results are in: " ++ show sudoku
  print output
