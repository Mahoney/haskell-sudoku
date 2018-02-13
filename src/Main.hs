module Main where

import System.Environment
import Sudoku.SimpleCli.Cli

main :: IO ()
main = do
  args <- getArgs
  putStrLn (app args)
