module Sudoku.SimpleCli.ValidationSpec where

import Test.Hspec
import Sudoku.Interfaces
import Sudoku.SimpleCli.Validation

spec :: Spec
spec = describe "validate" $ do
         it "returns a validation error if no input" $
           validate [] `shouldBe` Left "Takes one argument of type sudoku"

         it "returns a validation error if more than one input" $
           validate [
             ".................................................................................",
             "................................................................................."
             ] `shouldBe` Left "Takes one argument of type sudoku"

         it "returns a valid sudoku" $
           validate ["................................................................................."]
             `shouldBe` Right (Sudoku ".................................................................................")
