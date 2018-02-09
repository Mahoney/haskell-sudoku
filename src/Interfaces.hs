module Interfaces where

type ValidationError = String

newtype Sudoku = Sudoku String deriving Show

type Validator = [String] -> Either ValidationError Sudoku
type Solver = Sudoku -> [Sudoku]
type ResultFormatter = [Sudoku] -> String
