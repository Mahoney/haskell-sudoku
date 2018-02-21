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

         it "returns a validation error if input less than 81 chars long" $
           validate ["................................................................................"]
             `shouldBe` Left "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had 80"

         it "returns a validation error if input more than 81 chars long" $
           validate [".................................................................................."]
             `shouldBe` Left "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had 82"

         it "returns a valid empty sudoku" $
           validate ["................................................................................."]
             `shouldBe` Right emptyGrid

         it "returns a validation error if input contains invalid characters" $
           validate ["1.a.Z.2.........................................................................."]
             `shouldBe` Left "a is not . or 1-9; Z is not . or 1-9"

         it "returns a valid grid with some cells filled in" $
           validate ["......3.."++
                     "6...2.1.."++
                     ".328.7..."++
                     "....4..6."++
                     "4.69.85.7"++
                     ".9..7...."++
                     "...1.582."++
                     "..4.8...1"++
                     "..5......"]
             `shouldBe` Right (sudoku [ X,  X,  X,  X,  X,  X, I3,  X,  X,
                                       I6,  X,  X,  X, I2,  X, I1,  X,  X,
                                        X, I3, I2, I8,  X, I7,  X,  X,  X,
                                        X,  X,  X,  X, I4,  X,  X, I6,  X,
                                       I4,  X, I6, I9,  X, I8, I5,  X, I7,
                                        X, I9,  X,  X, I7,  X,  X,  X,  X,
                                        X,  X,  X, I1,  X, I5, I8, I2,  X,
                                        X,  X, I4,  X, I8,  X,  X,  X, I1,
                                        X,  X, I5,  X,  X,  X,  X,  X,  X])
