module Main where

import System.Environment
import Sudoku.Cli

main :: IO ()
main = do
  args <- getArgs
  putStrLn (app args)
