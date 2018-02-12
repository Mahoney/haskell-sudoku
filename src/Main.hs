module Main (main, app) where

import System.Environment
import Validation
import BusinessLogic

app :: [String] -> String
app = inMemoryTransaction validate solve show

main :: IO ()
main = do
  args <- getArgs
  putStrLn (app args)
