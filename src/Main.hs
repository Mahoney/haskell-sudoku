module Main where

import System.Environment
import Interfaces
import Validation
import BusinessLogic

applicationBuilder :: Validator -> Solver -> ResultFormatter -> [String] -> String
applicationBuilder validator solver formatter args =
  let validationResult = validator args
      result = fmap solver validationResult
  in either id formatter result

main :: IO ()
main = do
  args <- getArgs
  let app = applicationBuilder validate solve show
  putStrLn (app args)
