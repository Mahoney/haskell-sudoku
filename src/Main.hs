module Main where

import System.Environment (getArgs)
import Sudoku.SimpleCli.Cli (app)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (app args)
