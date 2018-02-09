module Main (main, applicationBuilder) where

import System.Environment
import Interfaces
import Validation
import BusinessLogic
import Ext.Data.Either

applicationBuilder :: Validator -> Solver -> ResultFormatter -> [String] -> String
applicationBuilder validator solver formatter args =
  let validationResult = validator args
      result = fmap solver validationResult
  in rightMerge formatter result

main :: IO ()
main = do
  args <- getArgs
  let app = applicationBuilder validate solve show
  putStrLn (app args)
